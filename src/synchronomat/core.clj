(ns synchronomat.core (:gen-class)
    (:refer-clojure :exclude [< <= = > >=])
    (:use [clojure.contrib.generic.comparison :only (< <= = > >= )]
          [synchronomat.import-utils]
          [clojure.contrib.duck-streams :only (reader)]
          [synchronomat.file-test-utils :only (path-seq file)])
 (:import [java.nio.file WatchService 
                         FileSystem
                         FileSystems
                         Paths
                         Path
                         CopyOption
                         LinkOption]
          [java.nio.file.attribute Attributes
                                   BasicFileAttributes]
          [java.io IOException]))

(import-static-fields java.nio.file.StandardCopyOption)

(defmethod > [java.lang.Comparable java.lang.Comparable] 
  [this that]
  (> (.compareTo this that) 0))

(defmethod = [java.lang.Comparable java.lang.Comparable] 
  [this that]
  (= 0 (.compareTo this that)))

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
      (.get Paths path-name))

(defn- copy-file 
      "Copies src path to dest path keeping file attributes
      and overwriting without prompting."
      [src dest]
      (try (.copyTo src dest keep-last-modified-and-overwrite)
           (catch IOException e (format "Could not copy: %s: %s %n" src e))))

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




