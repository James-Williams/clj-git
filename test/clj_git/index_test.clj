(ns clj-git.index-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer :all]
            [clj-git.util :refer :all]
            [clj-git.file :refer :all]
            [clj-git.repo :refer :all]
            [clj-git.index :refer :all]))

(deftest t-read-index
  (testing "Reading 'first_index' fixture into a struct")
    (with-redefs [git-root (fn [] "fixtures/1/")]
      (is (= (read-index)
             [ {:inode 2099386, :device 16777218, :filesize 0x5,
                :hash "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"
                :name "test_file"
                :ctime #inst "2016-01-22T19:21:34.000-00:00"
                :mtime #inst "2016-01-22T19:21:34.000-00:00"}])))
    )

(deftest t-build-index
  (testing "Single file index is correctly constructed"
    (is (= (byte-seq (build-index
            [ { :inode 1529592,
                :device 16777220,
                :filesize 16,
                :name "test_file",
                :ctime #inst "2016-06-22T09:29:27.000-00:00",
                :mtime #inst "2016-06-22T09:29:27.000-00:00",
                :hash "2d55082916969610e3c65cec1fc04766208f39d4"} ]))
            (byte-seq (read-file "fixtures/one_file.git/index"))))
  )
)

(deftest t-build-then-index
  (testing "Build then Read an index produces the input index data"
    (doseq [filepath ["fixtures/1/index"
                      "fixtures/2/index"
                      "fixtures/3/index"] ]
      (let [index-struct (read-index (read-file filepath))]
        (is (= (-> index-struct build-index read-index)
               index-struct))))
  )
)

(deftest t-file-tree
  (testing "Build file tree structure from example file list")
    (is (= (files-tree [{:name "test_file" :hash "A"}
                        {:name "test/clj_git/core_test.clj" :hash "B"}
                        {:name "test/clj_git/test" :hash "C"}])
           {"test_file"
              {:type :file
               :parent nil
               :hash "A"
               :children []}
            "test/clj_git/test"
              {:type :file
               :parent "test/clj_git"
               :hash "C"
               :children []}
            "test"
              {:type :dir
               :parent nil
               :children ["test/clj_git"]}
            "test/clj_git"
              {:type :dir
               :parent "test"
               :children ["test/clj_git/core_test.clj"
                          "test/clj_git/test"]}
            "test/clj_git/core_test.clj"
              {:type :file
               :parent "test/clj_git"
               :hash "B"
               :children []}}))
)

(deftest t-index-to-tree-objects
  (testing "fixture1 index hash the correct top level hash")
  (with-redefs [git-root (fn [] "fixtures/1/")]
    (is (= (first (index-to-tree-objects))
           "ca93b49848670d03b3968c8a481eca55f5fb2150")))
  (testing "fixture2 index hash the correct top level hash")
  (with-redefs [git-root (fn [] "fixtures/2/")]
    (is (= (first (index-to-tree-objects))
           "40951cd74421289606500f35d0c6ccb50b552b4b")))
  (testing "fixture3 index hash the correct top level hash")
  (with-redefs [git-root (fn [] "fixtures/3/")]
    (is (= (first (index-to-tree-objects))
           "4b90f754d0ddf46643fb5250b47cddfb43102f30")))
)

(deftest t-stage-file
  (let [filename "newfile"]
    (testing "New file get's added to index"
      (assert (not (.exists (clojure.java.io/as-file filename))))
      (spit filename "hello")
      (is (not (is-file-staged filename)))
      (stage-file filename)
      (is (is-file-staged filename))
      (ok-sh "git" "reset" filename)
      (clojure.java.io/delete-file filename)
    )
))

; TODO: UNSTABLE! Failure from test_file missing sometimes (test interference..)
(deftest t-checkout-file
  (let [file "LICENSE"]
    (testing "Deleted file can be restored from index"
      (assert (.exists (clojure.java.io/as-file file)))
      (assert (not (is-file-modified file)))
      (assert (not (is-file-staged file)))
      (let [contents (slurp file)]
        (clojure.java.io/delete-file file)
        (is (is-file-modified file))
        (checkout-file file)
        (is (not (is-file-modified file)))
        (spit file contents))
    )
  )
)

(deftest t-is-file-modified
  (let [file "test_file"]
    (testing (str file " is not modified to begin with")
      (is (not (is-file-modified file))))
    (testing "touching file does not make it modified"
      (ok-sh "touch" file)
      (is (not (is-file-modified file))))
    (testing "changing the file does make it modified"
      (let [file-contents (slurp file)]
        (spit file "A")
        (is (is-file-modified file))
        (spit file file-contents)))
    (testing "changing one byte of the file does make it modified"
      (let [file-contents (slurp file)]
        (spit file (str "A" (subs file-contents 1)))
        (assert (= (count file-contents) (file-size file)))
        (is (is-file-modified file))
        (spit file file-contents)))
    (testing (str file " is not modified at end of test")
      (is (not (is-file-modified file))))
  ))

(deftest t-is-file-staged
  (let [file "test_file"]
    (testing (str file " is not modified or staged to begin with")
      (is (not (is-file-modified file)))
      (is (not (is-file-staged file)))
    )
    (testing "adding a modification using git makes it staged and unmodified"
      (let [file-contents (slurp file)]
        (spit file "A")
        (assert (is-file-modified file))
        (stage-file file)
        (assert (not (is-file-modified file)))
        (is (is-file-staged file))
        (ok-sh "git" "reset" file) ; TODO: Write and use clojure functions
        (spit file file-contents)
    ))
    (testing (str file " is not modified or staged at end of test")
      (is (not (is-file-modified file)))
      (is (not (is-file-staged file)))
    )
  ))

(deftest t-list-all-files
  (testing ".git/config is not listed"
    (assert (.exists (clojure.java.io/as-file ".git/config")))
    (is (not (contains? (into #{} (list-all-files)) ".git/config")))
  )
)

(deftest t-list-files
  (testing "test_file is listed"
    (assert (.exists (clojure.java.io/as-file "test_file")))
    (is (contains? (into #{} (list-files)) "test_file"))
  )

  (testing ".git file ending is not masked"
    (assert (not (.exists (clojure.java.io/as-file "test.git"))))
    (ok-sh "touch" "test.git")
    (is (contains? (into #{} (list-files)) "test.git"))
    (clojure.java.io/delete-file "test.git")
  )

  (testing ".gitignore is not listed"
    (assert (.exists (clojure.java.io/as-file ".gitignore")))
    (is (not (contains? (into #{} (list-files)) ".gitignore")))
  )
)
