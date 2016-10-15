(ns clj-git.core-test
  (:require [clojure.test :refer :all]
            [clj-git.util :refer :all]
            [clj-git.file :refer :all]
            [clj-git.repo :refer :all]
            [clj-git.object :refer :all]
            [clj-git.core :refer :all]))

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

; TODO: Add tests for list-unignored-files and list-untracked-files
