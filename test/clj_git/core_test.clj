(ns clj-git.core-test
  (:require [clojure.test :refer :all]
            [clj-git.util :refer :all]
            [clj-git.file :refer :all]
            [clj-git.repo :refer :all]
            [clj-git.object :refer :all]
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

(deftest t-hash-tree
  (testing "Check that trees hash to original values")
    (is (= (hash-tree (tree "6811239b02050711578e223ffe1c297a0eb801f4"))
           "6811239b02050711578e223ffe1c297a0eb801f4"))
  (testing "Top level tree entry with both blob and tree children")
    (is (= (hash-tree (tree "4b90f754d0ddf46643fb5250b47cddfb43102f30"))
           "4b90f754d0ddf46643fb5250b47cddfb43102f30"))
  (testing "Reversing object list has no effect")
    (is (= (hash-tree (reverse
                        (tree "4b90f754d0ddf46643fb5250b47cddfb43102f30")))
           "4b90f754d0ddf46643fb5250b47cddfb43102f30"))
  (testing "Check that trees hash to original values")
    (is (= (hash-tree (tree "ca93"))
           "ca93b49848670d03b3968c8a481eca55f5fb2150"))
)

(deftest t-modified
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
