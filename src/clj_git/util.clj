(ns clj-git.util
  (:use clojure.java.shell)
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

(defn read-file [path]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream path) out)
    out))

(defn to-str [x] (apply str (map char x)))

(defn current-unix-time []
  (quot (System/currentTimeMillis) 1000))

(defn int-bytes
  ( [val len]
    (let [bs (int-bytes val)
          bs-len (count bs)]
      (assert (<= bs-len len))
      (concat (repeat (- len bs-len) 0) bs)))
  ( [val]
    (let [hex (format "%02x" val)
          even (if (even? (count hex)) hex (str "0" hex))
          pairs (partition 2 even)
          strs (map #(apply str %) pairs)
          nums (map #(read-string (str "0x" %)) strs)]
      nums)))

(defn hex-bytes [hex-str]
  (let [hex-strs (map #(apply str %) (partition 2 hex-str))
        hex-bs (map #(read-string (str "0x" %)) hex-strs)]
    hex-bs))

(def byte-seq #(->> % .toByteArray (map (fn [x] (mod x 256))) vec))

