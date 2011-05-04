(ns synchronomat.test.core (:gen-class)
 (:use [synchronomat.core] :reload)
 (:use [clojure.test] 
       [synchronomat.file-test-utils]
       [clojure.contrib.duck-streams :only (slurp*)]))


(deftest can-instantiate-watchservice 
  (is (not (nil? (create-watchservice)))))


(deftest can-copy-file-while-maintaining-last-modified-time
  (with-tmp-dirs [src  ["copy.me"]
                  dest []]
                 (let [src-file (subpath src "copy.me")
                       dest-file (subpath dest "copy.me")]
                   (Thread/sleep 1000)
                   (copy-folder src dest)
                   (is (containing-files? ["copy.me"] dest))
                   (is (= (last-modified-time src-file) 
                          (last-modified-time dest-file))))))

(deftest copy-overwrites-file-when-last-modification-of-src-file-newer
  (let [a-file-name "file_to_copy"] 
          (with-tmp-dirs [src [(comp (set-modified-to-fn 2000)
                                     (spit-into-fn "expected")
                                     (create-file-fn a-file-name))]
                          dest [(comp (set-modified-to-fn 1000)
                                      (spit-into-fn "overwrite me")
                                      (create-file-fn a-file-name))]]
                         (let [src-file (subpath src a-file-name)
                               dest-file (subpath dest a-file-name)]
                           (copy-folder src dest)
                           (is (= "expected" (slurp* (file dest-file))))))))


(deftest copy-does-not-overwrite-file-when-last-modification-equal
  (let [a-file-name "file_to_not_copy"] 
          (with-tmp-dirs [src [(comp (set-modified-to-fn 1000)
                                     (spit-into-fn "should not be copied")
                                     (create-file-fn a-file-name))]
                          dest [(comp (set-modified-to-fn 1000)
                                      (spit-into-fn "expected")
                                      (create-file-fn a-file-name))]]
                         (let [dest-file (subpath dest a-file-name)]
                           (copy-folder src dest)
                           (is (= "expected" (slurp* dest-file)))))))


(deftest can-copy-folder
         (with-tmp-dirs [src [['folder1 ]]
                        dest []]
                        (copy-folder src dest)
                        (is (dir? (subpath dest "folder1")))))

(deftest can-copy-folder-containing-files
         (with-tmp-dirs [src [['folder 
                                 'somefile
                                 'someotherfile ]]
                         dest []]
                        (copy-folder src dest)
                        (is (containing-files? ["folder" 
                                                "folder/somefile"
                                                "folder/someotherfile"]
                                               dest))))

(deftest delete-junk-removes-deleted-files
         (with-tmp-dirs [src ['[a 
                                 [b 
                                   f1]
                                 f2]
                              '[c f3]]
                         dest ['[a 
                                  [b f1 f5]
                                  f2]
                               '[c [d] 
                                   f0
                                   f3
                                   f6]
                               '[e x1 x2]]]
                (delete-junk src dest)
                (is (containing-files? ["a" 
                                        "a/b"
                                        "a/b/f1"
                                        "a/f2"
                                        "c"
                                        "c/f3"] dest))))

(deftest mirror-folder-copies-new-files-and-deletes-junk
         (with-tmp-dirs [src ['file1 ]
                         dest ['remove.me ]]
                        (mirror-folder src dest)
                        (is (containing-files? ["file1"] dest))))

#_(deftest sync-dir-copies-newly-created-file
         (with-tmp-dirs [src  []
                         dest []]
              (let [sync-future (sync-dir src dest) 
                    file-to-copy (create-file src "should.copy")]
                (Thread/sleep 1000) ;TODO: how can we wait for syncing??
                (future-cancel sync-future)
              (is (containing-files? ["should.copy"] dest))
              #_(is (= (last-modified-time file-to-copy)
                     (last-modified-time (subpath dest "should.copy")))))))





