(ns clj-git.object
  (:use clj-message-digest.core)
  (:use clojure.java.io)
  (:use clojure.java.shell)
  (:use clj-git.util)
  (:use clj-git.repo)
  (:require [clojure.java.io :as io])
  (:gen-class))

(def SHA-1-HEX-LENGTH 40)

(defn hash-str [text]
  (let [full-text (str "blob " (count text) "\0" text)]
    (sha-1-hex full-text)))

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

(defn compress-zlib [data]
  (let [buffer (byte-array (alength data))
        out (java.io.ByteArrayOutputStream.)
        in (java.util.zip.DeflaterInputStream. (java.io.ByteArrayInputStream. data)) ]
    (loop [bytes-read 0]
      (when-not (< bytes-read 0)
        (.write out buffer 0 bytes-read)
        (recur (.read in buffer))))
    (.flush out)
    (.close out)
    (.toByteArray out)))

(defn write-object [bs]
  (let [hash-text (sha-1-hex bs)
        [d-name f-name] (map #(apply str %) (split-at 2 hash-text))
        filepath (str (git-root) "objects/" d-name "/" f-name)
        data (byte-array (map byte bs))
        bdata (compress-zlib data)]
    (make-parents filepath)
    (if (not (.exists (clojure.java.io/as-file filepath)))
      (do
        (with-open [wrtr (output-stream filepath)]
          (.write wrtr bdata))
        (unset-writable filepath)))
    hash-text))

(defn write-blob [text]
  (write-object (str "blob " (count text) "\0" text)))

(defn write-commit [text]
  (write-object (str "commit " (count text) "\0" text)))

(defn hash-file [filepath]
  (hash-str (slurp filepath)))

;TODO: Use flags from entry hash-map
(defn tree-entry-bytes [entry]
  (let [flags-bs (if (= (:type entry) "tree")
                        '(52 48 48 48 48)
                        '(49 48 48 54 52 52))
        name-bs (->> (:name entry)
                     (map byte)
                     (byte-array)
                     (seq))
        hash-bs (->> (:hash entry)
                    (partition 2)
                    (map #(apply str %))
                    (map #(str "0x" %))
                    (map read-string)
                    (byte-array)
                    (seq))]
    (concat flags-bs
            '(32)
            name-bs
            '(00)
            hash-bs)))

(defn tree-data [tree-struct]
  (let [sorted (sort-by #(:name %) compare tree-struct)
        body (apply concat (map tree-entry-bytes sorted))
        size-bs (map byte (str (count body)))
        tree-str-bs (map byte "tree ")
        header-bs (concat tree-str-bs size-bs '(0))
        data (concat header-bs body)
        ]
    data))

(defn write-tree [tree-struct]
  (-> tree-struct
      (tree-data)
      (byte-array)
      (write-object)))

(defn hash-tree [tree-struct]
  (->> tree-struct
       (tree-data)
       (byte-array)
       (sha-1-hex)))

(defn all-objects []
  (let [object-path (str (git-root) "objects/")
        pref-files (drop 1 (file-seq (clojure.java.io/file object-path)))
        are-files (filter #(.isFile %) pref-files)
        hashes (map #(str (.getName (.getParentFile %))
                         (.getName %)) are-files)]
        
    (map #(do (assert (= (count %) SHA-1-HEX-LENGTH)) %) hashes)))

(defn complete-hash [hash-prefix]
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

(defn read-object [hash-prefix]
  (let [hash-hex (complete-hash hash-prefix)
        filepath (str (git-root) 
                      "objects/"
                      (subs hash-hex 0 2) "/" 
                      (subs hash-hex 2 (count hash-hex)))
        bs (seq (decompress-zlib (read-file filepath)))
        ix (inc (.indexOf bs (byte 0)))]
        (assert (> ix 0))
        (split-at ix bs)))

(defn object-type [hash-hex]
  (let [header (to-str (first (read-object hash-hex)))]
    (first (clojure.string/split header #" "))))

(defn tree [h]
  (let [[header ins] (read-object h)
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
    (assert (= "tree" (apply str (take 4 (map char header)))))
    (loop [[e r] (f-entry ins) out []]
      (if (empty? r)
        (conj out e)
        (recur (f-entry r) (conj out e))))))

(defn blob [hash-hex]
  (to-str (second (read-object hash-hex))))

(def commit blob)
