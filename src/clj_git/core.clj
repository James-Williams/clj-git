(ns clj-git.core
  (:use clj-message-digest.core)
  (:require [clojure.java.io :as io])
  (:gen-class))

(def SHA-1-HEX-LENGTH 40)

(defn hash-blob
  [text]
  (let [full-text (str "blob " (count text) "\0" text)]
    (sha-1-hex full-text)))

(defn git-root [] "./")

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

(defn all-objects []
  (let [object-path (str (git-root) ".git/objects/")
        pref-files (drop 1 (file-seq (clojure.java.io/file object-path)))
        are-files (filter #(.isFile %) pref-files)
        hashes (map #(str (.getName (.getParentFile %))
                         (.getName %)) are-files)]
        
    (map #(do (assert (= (count %) SHA-1-HEX-LENGTH)) %) hashes)))

(defn complete-hash
  [hash-prefix]
  (assert (<= (count hash-prefix) SHA-1-HEX-LENGTH))
  (let [len (count hash-prefix)
        all (all-objects)
        ps (map #(subs % 0 len) all)
        pairs (map list ps all)
        matches (filter (fn [[x _]] (= x hash-prefix)) pairs)]
    (cond
      (empty? matches)
      (throw (Exception. 
               (str "No matching objects for prefix '" hash-prefix "'")))
      (> (count matches) 1)
      (throw (Exception. 
               (str "Ambiguous object prefix '" hash-prefix "'")))
      :else (second (first matches)))))

(defn read-object
  [hash-prefix]
  (let [hash-hex (complete-hash hash-prefix)
        filepath (str (git-root) 
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

(defn branch
  [b]
  (let [filepath (str (git-root) ".git/refs/heads/" b)]
    (clojure.string/replace (slurp filepath) #"\n" "")))

