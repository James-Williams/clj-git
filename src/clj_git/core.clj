(ns clj-git.core
  (:use clj-message-digest.core)
  (:require [clojure.java.io :as io])
  (:gen-class))

(def SHA-1-HEX-LENGTH 40)

(defn to-str [x] (apply str (map char x)))

(defn hash-blob
  [text]
  (let [full-text (str "blob " (count text) "\0" text)]
    (sha-1-hex full-text)))

(defn git-root [] "./.git/")

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
  (let [object-path (str (git-root) "objects/")
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
                      "objects/"
                      (subs hash-hex 0 2) "/" 
                      (subs hash-hex 2 (count hash-hex)))
        bs (seq (decompress-zlib (read-file filepath)))
        ix (inc (.indexOf bs (byte 0)))]
        (assert (> ix 0))
        (split-at ix bs)))

(defn object-type
  [hash-hex]
  (let [header (to-str (first (read-object hash-hex)))]
    (first (clojure.string/split header #" "))))

(defn tree
  [h]
  (let [[_ ins] (read-object h)
        f-entry (fn [bs]
                  (let [spacex (.indexOf bs (int \space))
                        flags (take spacex bs)
                        nxt (drop (inc spacex) bs)
                        nullx (.indexOf nxt 0)
                        name (take nullx nxt)
                        remain (drop (inc nullx) nxt)
                        len (/ SHA-1-HEX-LENGTH 2)
                        hash-vals (take len remain)
                        nxt-entry (drop len remain)
                        to-hex #(format "%02x" (mod % 256))
                        hash-str (clojure.string/join
                                   (map to-hex hash-vals))
                        obj-type (object-type hash-str)]
                    (list { :name (to-str name)
                            :flags (to-str flags)
                            :type obj-type
                            :hash hash-str}  nxt-entry)))]
    (loop [[e r] (f-entry ins) out []]
      (if (empty? r)
        (conj out e)
        (recur (f-entry r) (conj out e))))))

(defn blob
  [hash-hex]
  (to-str (second (read-object hash-hex))))

(defn branch
  [b]
  (let [filepath (str (git-root) "refs/heads/" b)]
    (clojure.string/replace (slurp filepath) #"\n" "")))

(defn read-index []
  (let [filepath (str (git-root) "index")
        bs (seq (read-file filepath))
        hs (vec (map #(format "%02x" (mod % 256)) bs))
        es (drop 12 bs)
        to-num (fn [x] (->> x
                           (clojure.string/join)
                           (str "0x")
                           (read-string)))]
    [{:inode (to-num (subvec hs 0x20 0x24)),
     :device (to-num (subvec hs 0x24 0x28)),
     :filesize (to-num (subvec hs 0x30 0x34)),
     :hash (clojure.string/join (subvec hs 0x34 0x48))}]))

