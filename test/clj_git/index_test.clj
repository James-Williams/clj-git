(ns clj-git.index-test
  (:require [clojure.test :refer :all]
            [clj-git.util :refer :all]
            [clj-git.repo :refer :all]
            [clj-git.index :refer :all]))

(deftest t-read-index
  (testing "Reading 'first_index' fixture into a struct")
    (with-redefs [git-root (fn [] "test/fixture1/")]
      (is (= (read-index)
             [ {:inode 2099386, :device 16777218, :filesize 0x5,
                :hash "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"
                :name "test_file"
                :ctime #inst "2016-01-22T19:21:34.000-00:00"
                :mtime #inst "2016-01-22T19:21:34.000-00:00"}])))
    )

(deftest t-build-index
  (testing "Single file index is correctly constructed")
  (is (= (byte-seq (build-index
          [ { :inode 1529592,
              :device 16777220,
              :filesize 16,
              :name "test_file",
              :ctime #inst "2016-06-22T09:29:27.000-00:00",
              :mtime #inst "2016-06-22T09:29:27.000-00:00",
              :hash "2d55082916969610e3c65cec1fc04766208f39d4"} ]))
          (byte-seq (read-file "test/fixture_one_file/index")))))

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
  (with-redefs [git-root (fn [] "test/fixture1/")]
    (is (= (first (index-to-tree-objects))
           "ca93b49848670d03b3968c8a481eca55f5fb2150")))
  (testing "fixture2 index hash the correct top level hash")
  (with-redefs [git-root (fn [] "test/fixture2/")]
    (is (= (first (index-to-tree-objects))
           "40951cd74421289606500f35d0c6ccb50b552b4b")))
  (testing "fixture3 index hash the correct top level hash")
  (with-redefs [git-root (fn [] "test/fixture3/")]
    (is (= (first (index-to-tree-objects))
           "4b90f754d0ddf46643fb5250b47cddfb43102f30")))
)
