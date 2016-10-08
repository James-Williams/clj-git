(ns clj-git.util-test
  (:require [clojure.test :refer :all]
            [clj-git.util :refer :all]))

(deftest t-int-bytes
    (is (= (int-bytes 1) [1]))
    (is (= (int-bytes 255) [255]))
    (is (= (int-bytes 256) [1 0]))
    (is (= (int-bytes 0xABCDEF) [0xAB 0xCD 0xEF]))
    (is (= (int-bytes 0x9ABCDEF) [0x09 0xAB 0xCD 0xEF]))
    (is (= (int-bytes 1 4) [0 0 0 1]))
    (is (= (int-bytes 2099386 8) [0 0 0 0 0 32 8 186]))
    )

(deftest t-glob-match?
  (testing "Basic glob rules"
    (is (glob-match? "hi" "hi"))
    (is (glob-match? "/hi" "hi"))
    (is (glob-match? "a*c" "abc"))
    (is (glob-match? "a*c" "ab9c"))
    (is (glob-match? "a*c" "abcd"))
    (is (glob-match? "a*c" "zabcd"))
    (is (not (glob-match? "/a*c" "zabcd")))
    (is (not (glob-match? "a*c" "ab/c")))
  )
  (testing "Boolean output"
    (is (= (glob-match? "hi" "hi") true))
    (is (= (glob-match? "hi" "ho") false))
  )
  (testing "Specific .gitignore cases"
    (is (glob-match? "/.git/" ".git/config"))
  )
)
