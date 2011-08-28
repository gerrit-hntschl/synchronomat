(ns synchronomat.core (:gen-class)
   (:refer-clojure :exclude [< <= = > >= sync])
    (:use [clojure.contrib.generic.comparison :only (< <= = > >= )]
          [synchronomat.import-utils :only (import-static-fields)]
          [clojure.contrib.import-static :only (import-static)]
          [clojure.contrib.java-utils :only (delete-file-recursively)]
          [clojure.contrib.duck-streams :only (reader)]
          [clojure.contrib.seq-utils :only (fill-queue)]
          [synchronomat.file-test-utils :only (path-seq file dir?)])
 (:import [java.nio.file WatchService 
                         FileSystem
                         FileSystems
                         Files
                         Paths
                         Path
                         SimpleFileVisitor
                         CopyOption
                         LinkOption]
          [java.nio.file.attribute Attributes
                                   BasicFileAttributes]
          [java.io IOException]
          [java.util.concurrent DelayQueue Delayed TimeUnit]))

(import-static-fields java.nio.file.StandardCopyOption)
(import-static java.nio.file.StandardWatchEventKind ENTRY_CREATE ENTRY_DELETE ENTRY_MODIFY OVERFLOW)
(import-static java.nio.file.FileVisitResult CONTINUE SKIP_SUBTREE)

(def all-watch-events (into-array java.nio.file.WatchEvent$Kind [ENTRY_CREATE 
                                                                ENTRY_MODIFY 
                                                                ENTRY_DELETE
                                                                OVERFLOW]))


(defmethod > [java.lang.Comparable java.lang.Comparable] 
  [this that]
  (> (.compareTo this that) 0))

(defmethod = [java.lang.Comparable java.lang.Comparable] 
  [this that]
  (== 0 (.compareTo this that)))

(defmethod < [java.lang.Comparable java.lang.Comparable] 
  [this that]
  (< (.compareTo this that) 0))

(prefer-method > [java.lang.Number java.lang.Number] 
                 [java.lang.Comparable java.lang.Comparable])

(prefer-method = [java.lang.Number java.lang.Number] 
                 [java.lang.Comparable java.lang.Comparable])

(prefer-method < [java.lang.Number java.lang.Number] 
                 [java.lang.Comparable java.lang.Comparable])

(defmethod reader Path [p]
  (reader (file p)))


(def keep-last-modified-and-overwrite
     (into-array CopyOption [COPY_ATTRIBUTES REPLACE_EXISTING]))

(def no-link-option
     (make-array LinkOption 0))

(defn last-modified-time [path]
      (.lastModifiedTime (Attributes/readBasicFileAttributes
                           path no-link-option)))

(defn create-watchservice []
  (.. FileSystems getDefault newWatchService))

(defn path [path-name]
      (Paths/get path-name))

(defn- copy-file 
      "Copies src path to dest path keeping file attributes
      and overwriting without prompting."
      [src dest]
      (try (.copyTo src dest keep-last-modified-and-overwrite)
           (catch IOException e (println (format "Could not copy: %s: %s %n" src e)))))

(defn copy-folder 
      "Copies all files from the src folder into the dest folder.
      Overwrites older files in dest folder without
      prompting and keeps files with same modification date."
      [src-folder dest-folder]
      (doseq [src-path (path-seq src-folder)]
             (let [target-path (.resolve dest-folder 
                                         (.relativize src-folder src-path))]
               (when (or 
                       (not (.exists target-path))
                       (> (last-modified-time src-path) 
                          (last-modified-time target-path)))
                 (copy-file src-path target-path)))))

(defn delete-junk
      "Removes all files and folders that are below dest 
      but not in src"
      [src-folder dest-folder]
      (doseq [dest-path (path-seq dest-folder)]
             (let [src-path (.resolve src-folder 
                                      (.relativize dest-folder dest-path))]
               (when (and (.exists dest-path) ; necessary because file might have been deleted already?
                          (not (.exists src-path)))
                 (delete-file-recursively (file dest-path))))))

(defn mirror-folder
      "Mirrors all files below src in dest."
      [src-folder dest-folder]
      (do 
        (copy-folder src-folder dest-folder)
        (delete-junk src-folder dest-folder)))



(defn register 
      "Registers the watcher for event-kinds at dir. Returns watchkey
      for the directory"
      [^WatchService watcher event-kinds dir]
      (.register dir watcher event-kinds))

(defn register-all
      [watchkey->dir dir-to-watch watch-service]
      (let [reg-dir (partial register watch-service all-watch-events)
            visitor (proxy [SimpleFileVisitor] []
                      (preVisitDirectory [dir atts]
                          (try
                            (swap! watchkey->dir assoc 
                                   (reg-dir dir) dir)
                            #_(println @watchkey->dir)
                            CONTINUE
                            (catch IOException e 
                                   ;TODO add errors to error queue??
                                   (println (format "could not register dir: %s -> %s" dir e))
                                   SKIP_SUBTREE))))]
            (Files/walkFileTree dir-to-watch visitor))) 

(defn convert-event
      "Takes a WatchEvent and a dir and converts it into a map
      containing a key specifying the kind of event
      and the file that is affected by the event."
      [dir event]
      {:type (condp = (.kind event)
             ENTRY_CREATE :created
             ENTRY_MODIFY :modified
             ENTRY_DELETE :deleted
             OVERFLOW :overflowed) 
        :path (.resolve dir (.context event)) })


(defn register-created-dirs 
      "Takes a watchservice and a map containing the event-type
      and affected dir and registers the dir at the watchservice
      if dir was created. Returns the event."
      [watchkey->dir watcher converted-event]
      (do 
        (when (= :created (:type converted-event))
          (when-let [created-dir (dir? (:path converted-event))]
                  (try 
                    (register-all watchkey->dir created-dir watcher)
                    (catch IOException e (println "AAAH"))))) 
        converted-event))

(defn reset-key
      "Resets key and removes invalid keys from keymap"
      [key watchkey->dir]
      (let [valid (.reset key)]
        (when-not valid
                  (swap! watchkey->dir dissoc key))))


(defn dir-event-seq
      "Creates a lazy seq that is filled with directory events
      that occured in dir"
      [^Path dir-to-watch]
      (let [watcher (create-watchservice)
            watchkey->dir (atom {})]
        (register-all watchkey->dir dir-to-watch watcher)
        (fill-queue (fn [fill]
                        (while true 
                          (let [key (.take watcher)
                                watched-dir (@watchkey->dir key)
                                events (.pollEvents key)] 
                            (doseq [event events]
                                   (->> 
                                     (convert-event watched-dir event)  
                                     (register-created-dirs watchkey->dir watcher)
                                     (fill)) 
                                   (reset-key key watchkey->dir))))))))


(defn event->fn "Returns a map from event-type to function "
      [src dest]
      (letfn [(src->dest [src-path]
                                     (.resolve dest
                                         (.relativize src src-path)))
              (copy-relative [path]
                             (do (println "copying" path (System/currentTimeMillis))
                               (copy-file path 
                                              (src->dest path)))
                                   )
              (delete-relative [deleted-path]
                               (do
                                 (println "deleting" deleted-path (System/currentTimeMillis))
                                 (delete-file-recursively 
                                    (file (src->dest deleted-path))
                                    "silently"))
                                  )]
      (into {} (list [:created copy-relative]
                     [:modified copy-relative]
                     [:deleted delete-relative]))))



(defn delay-queue [] (DelayQueue. ))

(defprotocol SyncAction 
             (sync-file [this]))

(deftype DelayedSync [sync-f path creation-nanosecs]
           Delayed
           (getDelay [this time-unit]
                     (.convert time-unit 
                               (- (+ (.toNanos TimeUnit/SECONDS 1)
                                     creation-nanosecs)
                                  (System/nanoTime))
                               TimeUnit/NANOSECONDS))
           (compareTo [this that]
                      (compare creation-nanosecs (.creation-nanosecs that)))
           SyncAction
           (sync-file [this] (sync-f path))
           Object
           (equals [this that]
                   (= path (.path that)))
           (hashCode [this]
                     (.hashCode path)))


(defn enqueue-action [delay-queue action path]
      (let [dsync (DelayedSync. action path (System/nanoTime))]
        (while (.remove delay-queue dsync))
        (.put delay-queue dsync)) )

(defn sync-dir
      "Copies every file that is created or modified in src into dest"
      [src dest]
      (let [action-table (event->fn src dest)
                         dq (delay-queue) ]
      [(future ;don't block this thread
        (doseq [event (dir-event-seq src)]
               (println "event" event (System/currentTimeMillis))
               (enqueue-action dq (action-table (:type event)) (:path event))))
       (future 
         (do 
           (println "listening...") 
           (while true
               (let [task (.take dq)]
                 (println "syncing" task (System/currentTimeMillis))
               (sync-file task))))
        )]))

(defn -main [source dest & _]
      (let [src (path source)
            dest (path dest)]
        (cond 
          (not (.exists src)) (throw (IllegalArgumentException. "Source folder does not exist")) 
          (not (.exists dest)) (throw (IllegalArgumentException. "Destination folder does not exist")) 
          :else (sync-dir src dest))))


