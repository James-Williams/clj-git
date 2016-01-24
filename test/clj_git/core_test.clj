(ns clj-git.core-test
  (:require [clojure.test :refer :all]
            [clj-git.core :refer :all]))

(deftest t-hash-str
  (testing "Empty Str"
    (is (= (hash-str "") "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391")))
  (testing "Test Str"
    (is (= (hash-str "test\n") "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")))
  )
(deftest t-test-blob
  (testing "Object Content"
    (is (= (blob "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")
           "test\n")))
  (testing "Object Type"
    (is (= (object-type "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")
           "blob")))
  )

(deftest t-zlib
  (testing "compress->decompress 'hello'")
    (is (= (seq (decompress-zlib (compress-zlib 
                              (byte-array (map byte "hello")))))
           (seq (byte-array (map byte "hello")))))
  )

(deftest t-hash-file
  (testing "Hash test_file")
    (is (= (hash-file "test_file")
           "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")))

(deftest t-write-blob
  (testing "Store test_file blob")
    (is (= (write-blob (slurp "test_file"))
           "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")))

(deftest t-complete-hash
  (testing "Complete '9daead'"
    (is (= (complete-hash "9daeaf")
            "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"))))

(deftest t-branch-lookup
  (testing "Lookup branch 'first'"
    (is (= (branch "first")
            "bfac6b45a0b4086a308a1d1deac59fef612917f8"))))

(deftest t-tree
  (testing "Two entry tree for ./test/clj-git/"
    (is (= (tree "47254fa494e4ab6556b5f434fb2575d0e0c3ff17")
            [ {:type "blob" :name "core_test.clj" :flags "100644"
               :hash "9348f6141516da61950a85f844d858f172475e82"}
              {:type "blob" :name "temp" :flags "100644"
               :hash "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"}])))
  )

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

(deftest t-file-tree
  (testing "Build file tree structure from example file list")
    (is (= (files-tree ["test_file"
                             "test/clj_git/core_test.clj"])
           {"test_file"
              {:type :file,
               :parent nil},
            "test"
              {:type :dir
               :parent nil},
            "test/clj_git"
              {:type :dir
               :parent "test"},
            "test/clj_git/core_test.clj"
              {:type :file,
               :parent "test/clj_git"}})))

(deftest t-hash-tree
  (testing "Check that trees hash to original values")
    (is (= (hash-tree (tree "6811239b02050711578e223ffe1c297a0eb801f4"))
           "6811239b02050711578e223ffe1c297a0eb801f4"))
  (testing "Check that trees hash to original values")
    (is (= (hash-tree (tree "ca93"))
           "ca93b49848670d03b3968c8a481eca55f5fb2150"))
)

                              

