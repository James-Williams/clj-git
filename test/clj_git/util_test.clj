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
