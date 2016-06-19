(ns clj-git.util
  (:use clojure.java.io)
  (:use clojure.java.shell)
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn ok-sh [command & args]
  (let [res (apply sh (cons command args))]
    (assert (= 0 (:exit res)))
    (assert (= "" (:err res)))
    (:out res)))

(defn set-writable [filepath]
  (ok-sh "chmod" "u+w" filepath))

(defn unset-writable [filepath]
  (ok-sh "chmod" "u-w" filepath))

(defn to-str [x] (apply str (map char x)))
