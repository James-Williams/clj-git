(ns clj-git.core
  (:use clj-message-digest.core)
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn hash-blob
  [text]
  (let [full-text (str "blob " (count text) "\0" text)]
    (sha-1-hex full-text)))

(defn find-git-root [] "./")

(defn read-file [path]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream path) out)
    (.toByteArray out)))

(defn decompress-zlib [data]
  (let [buffer (byte-array (alength data))
        out (java.io.ByteArrayOutputStream.)
        in (java.util.zip.InflaterInputStream. (java.io.ByteArrayInputStream. data)) ]
    (loop [bytes-read 0]
      (when-not (< bytes-read 0)
        (.write out buffer 0 bytes-read)
        (recur (.read in buffer))))
    (.flush out)
    (.close out)
    (.toByteArray out)))

(defn read-object
  [hash-hex]
  (let [filepath (str (find-git-root) 
                      ".git/objects/" 
                      (subs hash-hex 0 2) "/" 
                      (subs hash-hex 2 (count hash-hex)))
        bs (seq (decompress-zlib (read-file filepath)))
        to-str (fn [x] (apply str (map char x)))]
        (map to-str (split-at (inc (.indexOf bs (byte 0))) bs))))

(defn object-content
  [hash-hex]
  (second (read-object hash-hex)))

(defn object-type
  [hash-hex]
  (let [header (first (read-object hash-hex))]
    (first (clojure.string/split header #" "))))
