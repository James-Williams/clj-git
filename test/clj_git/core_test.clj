(ns clj-git.core-test
  (:require [clojure.test :refer :all]
            [clj-git.core :refer :all]))

(deftest t-hash-blob
  (testing "Empty Blob"
    (is (= (hash-blob "") "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391")))
  (testing "Test Blob"
    (is (= (hash-blob "test\n") "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")))
  )
(deftest t-test-blob
  (testing "Object Content"
    (is (= (object-content "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")
           "test\n")))
  (testing "Object Type"
    (is (= (object-type "9daeafb9864cf43055ae93beb0afd6c7d144bfa4")
           "blob")))
  )

(deftest t-complete-hash
  (testing "Complete '9daead'"
    (is (= (complete-hash "9daeaf")
            "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"))))

(deftest t-branch-lookup
  (testing "Lookup branch 'first'"
    (is (= (branch "first")
            "bfac6b45a0b4086a308a1d1deac59fef612917f8"))))
