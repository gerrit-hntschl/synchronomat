(ns synchronomat.file-test-utils
    (:use [clojure.contrib.java-utils :only (delete-file-recursively)])
    (:import [java.io File] 
             [java.nio.file.attribute FileAttribute
                                      Attributes
                                      FileTime]
             [java.nio.file Path 
                            Files]))


; empty FileAttribute array needed for invocation of java vararg methods
(def no-atts (into-array FileAttribute []))

(defn create-tmp-folder 
      "Creates a temporary folder and returns a path to the created folder."
      []
      (let [dir (File/createTempFile "clj-tmptestfolder" "")]
        (doto dir
            (.delete)
            (.mkdir))
        (.toPath dir)))


(defn subpath 
      "Concatenates the relative path with 
      the parent and returns the resulting path"
      [#^Path parent #^String rel-path-name]
      (.resolve parent rel-path-name))

(defn create-dir 
      "Creates a directory for the given path and returns the path"
      ([path] 
       (do 
          (Files/createDirectories path no-atts)
          path))
      ([path subfolder-name]
       (create-dir (subpath path subfolder-name ))))





(defn create-file 
      "Creates a file from the given path"
      ([path] (.createFile path no-atts))
      ([path file-name] (create-file (subpath path file-name))))


; TODO: these look like monad stuff
(defn create-file-fn
      "Takes a file name and returns a function
      that takes a path and when applied 
      creates the file relative to the path"
      [file-name]
      (fn [path] (create-file path file-name)))



(defn file-tree 
      "Creates a file tree below dir given as nested coll
      and returns dir."
      [dir coll]
      (do  
        (doall ;make lazy map eager to perform side-effect creation of directories
            (map 
              (fn [p] 
                  (cond 
                    (coll? p) (when (seq p) 
                                (file-tree 
                                  (create-dir dir (str (first p)) )
                                  (next p)))
                    (fn? p) (p dir)
                    :else (create-file dir (str p))))
              coll)
            )
        dir))

(defn file ^File [^Path path]
      (File. (.toUri path)))

(defn set-modified-to [path millis]
      (do  
        (Attributes/setLastModifiedTime 
          path 
          (FileTime/fromMillis millis))
        path))

; TODO: these look like monad stuff
(defn set-modified-to-fn
      "Creates a function that takes
      a path and sets the last-modified-time
      of the path to millis"
      [millis]
      (fn [path]
          (set-modified-to path millis)))


(defn spit-into
      "Like clojure.core/spit but takes a path
      and returns it."
      [path content]
      (do
        (spit (file path) content ) 
        path))

; TODO: these look like monad stuff
(defn spit-into-fn
      "Creates a function that takes a path
      and writes the content into that path
      when applied"
      [content]
      (fn [path] (spit-into path content)))


(defn path-seq [path]
      (map (memfn toPath) (file-seq (file path))))

(defmacro with-tmp-dirs 
  "
  bindings => [name folder-tree]
  Binds each name to a temporary directory and creates subfolders
  and files as given by the
  nested seq. The temp folder and all content is deleted when leaving the function.
  Example usage:
  (with-tmp-dirs [src [['folder1
                          'foo.txt
                          'bar.zip]
                      ['emptyfolder]
                      'top-level-file]] ... )
  The first vector entry denotes the folder name and all subsequent names are files
  or nested vectors denoting subfolders. "
  [bindings & body]
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let [~(bindings 0) (file-tree (create-tmp-folder) ~(bindings 1))]
                              (try 
                                (with-tmp-dirs ~(subvec bindings 2) ~@body)
                                (finally
                                  (delete-file-recursively (file ~(bindings 0))))))
   ; :else (throw IllegalArgumentException. "with-tmp-dirs only allows Symbols in bindings")
    ))



(defn containing-files? 
      "Tests whether the given path to dir contains files named after collection entries. Filenames must
      appear in sequence as returned by path-seq called on dir."
      [coll dir]
      (= (path-seq dir) 
         (cons dir (map 
                     (fn [file-name] (.resolve dir file-name))
                     coll))))


(defn dir? 
      "Tests whether the given path represents an existing directory"
      [path]
      (.isDirectory (file path)))


