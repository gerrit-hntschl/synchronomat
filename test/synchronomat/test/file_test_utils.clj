(ns synchronomat.test.file-test-utils
  (:use [synchronomat.file-test-utils] :reload)
  (:use [clojure.test]
        [clojure.contrib.duck-streams :only (slurp*)]
        [clojure.contrib.io :only (delete-file-recursively)]
        [synchronomat.core])
  (:import [java.nio.file.attribute FileTime]
           [java.io File]))

(use-fixtures :each (fn clear-tmpdir-before-test [test]
                        (do
                          (dorun 
                            (->> (System/getProperty "java.io.tmpdir")
                              (File.) 
                              (.listFiles)
                              (filter (fn [file] 
                                          (.contains (.getAbsolutePath file) TMP-FOLDER-NAME))) 
                              (map delete-file-recursively)))
                          (test))))

(deftest can-create-tmp-folder
         (is (not (nil? (create-tmp-folder)) ) )) 

(deftest can-create-empty-tmp-folder 
         (is (file-tree 
                     (create-tmp-folder) 
                     [])))

(deftest can-create-file-tree-containing-nothing
         (is (containing-files? []
                                (file-tree (create-tmp-folder) []))))


(deftest can-create-file-tree-containing-nothing-with-empty-coll
         (is (containing-files? []
                                (file-tree (create-tmp-folder) [[]]))))

(deftest can-create-file-tree-containing-one-file
         (is (containing-files? ["f1.txt"]
                                (file-tree (create-tmp-folder) ["f1.txt"]))))

(deftest can-create-file-tree-containing-a-dir
         (is (containing-files? ["directory1"]
                                (file-tree (create-tmp-folder) [["directory1"]]))))

(deftest can-create-file-tree-containing-bunch-of-dirs-and-files
         (is (containing-files? ["d1" "d1/bar" "d1/foo" "d2" "xxxfile"]
                                (file-tree (create-tmp-folder) [["d1" 
                                                                    "foo" 
                                                                    "bar"]
                                                                ["d2"]
                                                                "xxxfile"]))))

(deftest can-create-file-tree-with-functions
         (is (containing-files? ["d1" "d1/f1.txt"]
                (file-tree (create-tmp-folder)
                  [["d1" (create-file-fn "f1.txt")]]))))


(deftest can-create-file-and-set-its-last-modified-time
         (let [f (-> (create-file (create-tmp-folder) "fake.fil")
                     (set-modified-to 1000))]
           (is (= (FileTime/fromMillis 1000) (last-modified-time f)))))


(deftest can-create-file-and-write-to-it
         (let [p (-> (create-file (create-tmp-folder) "scar.foo")
                     (spit-into "write me"))]
           (is (= "write me" (slurp* (file p))))))




