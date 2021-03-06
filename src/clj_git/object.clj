(ns clj-git.object
  (:use clj-message-digest.core)
  (:require [clojure.java.io :as io])
  (:require [clojure.spec.alpha :as s])
  (:require [clojure.spec.test.alpha :as stest])
  (:use clj-git.util)
  (:use clj-git.repo)
  (:gen-class))

(def SHA-1-HEX-LENGTH 40)

(s/check-asserts true)

(s/def ::sha-1-hash
  (s/and
    string?
    #(= (count %) SHA-1-HEX-LENGTH)))

(s/def ::sha-1-prefix
  (s/and
    string?
    #(<= (count %) SHA-1-HEX-LENGTH)
    #(> (count %) 0)
))

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
    (io/make-parents filepath)
    (if (not (.exists (io/as-file filepath)))
      (do
        (with-open [wrtr (io/output-stream filepath)]
          (.write wrtr bdata))
        (unset-writable filepath)))
    hash-text))

(defn hash-blob [payload]
  (let [payload-byte-length (count payload)
        header-bytes        (.getBytes (str "blob " payload-byte-length "\000"))
        full-bytes          (concat (seq header-bytes) (seq payload))]
    (sha-1-hex (byte-array full-bytes))))

(defn write-blob [payload]
  (let [payload-byte-length (count payload)
        header-bytes        (.getBytes (str "blob " payload-byte-length "\000"))
        full-bytes          (byte-array (concat (seq header-bytes) (seq payload)))]
    (write-object full-bytes)))

(defn write-file-blob [filename]
  (write-blob (.toByteArray (read-file (str (repo-root) filename)))))

(defn hash-file [filepath]
  (hash-blob (.toByteArray (read-file (str (repo-root) filepath)))))

; TODO: Test this works with unicode chars in message string
; TODO: Currently no tests for this function..
(defn write-commit [text]
  (write-object (.getBytes (str "commit " (count text) "\0" text))))

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

(defn is-valid-hash-str [s]
  (s/valid? ::sha-1-hash s))

(defn all-objects []
  (let [object-path (str (git-root) "objects/")
        pref-files (drop 1 (file-seq (io/file object-path)))
        are-files (filter #(.isFile %) pref-files)
        names (map #(str (.getName (.getParentFile %))
                         (.getName %)) are-files)]
    (filter is-valid-hash-str names)))

(s/fdef complete-hash
  :args (s/cat :hash-prefix ::sha-1-prefix)
  :ret ::sha-1-hash
)

(defn complete-hash [hash-prefix]
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

(stest/instrument `complete-hash)

(defn read-object [hash-prefix]
  (let [hash-hex (complete-hash hash-prefix)
        filepath (str (git-root)
                      "objects/"
                      (subs hash-hex 0 2) "/" 
                      (subs hash-hex 2 (count hash-hex)))
        bs (-> filepath read-file .toByteArray decompress-zlib seq)
        ix (inc (.indexOf bs (byte 0)))]
        (assert (> ix 0))
        (split-at ix bs)))

(defn object-type [hash-hex]
  (let [header (to-str (first (read-object hash-hex)))]
    (first (clojure.string/split header #" "))))

; TODO: This takes ~250ms!
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
  (->> hash-hex
    read-object
    second
    byte-array))

(defn commit [hash-str]
  (let [text  (String. (blob hash-str))
        lines (clojure.string/split text #"\n")
        [header body] (split-with #(not= % "") lines)
        pairs'(->> header
                (map #(clojure.string/split % #" "))
                (map #(list (first %) (clojure.string/join " " (rest %))))
              )
        parents (map second (filter #(= (first %) "parent") pairs'))
        pairs (filter #(not= (first %) "parent") pairs')
        commit (clojure.string/join "\n" (rest body))]
    (reduce (fn [entries pair]
              (assoc entries (keyword (first pair)) (second pair)))
            {:message commit, :parents parents}
            pairs)))
