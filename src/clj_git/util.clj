(ns clj-git.util
  (:use clojure.java.shell)
  (:gen-class))

(defn ok-sh [command & args]
  (let [res (apply sh (cons command args))]
    (assert (= 0 (:exit res)))
    (assert (= "" (:err res)))
    (:out res)))

(defn set-writable [filepath]
  (let [abs-path (.getAbsolutePath (clojure.java.io/as-file filepath))]
    (ok-sh "chmod" "u+w" abs-path)))

(defn unset-writable [filepath]
  (let [abs-path (.getAbsolutePath (clojure.java.io/as-file filepath))]
    (ok-sh "chmod" "u-w" abs-path)))

(defn read-file [path]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream path) out)
    out))

(defn write-file [path payload]
  (with-open [out (java.io.BufferedOutputStream. (java.io.FileOutputStream. path))]
    (.write out payload)))

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

(defn glob-to-regex-str
  "Takes a glob-format string and returns a regex-format string."
  [s]
  (loop [stream s
         re ""
         curly-depth 0]
    (let [[c j] stream]
        (cond
         (nil? c) (str (if (= \. (first s)) "" "(?=[^\\.])") re)
         (= c \\) (recur (nnext stream) (str re c c) curly-depth)
         (= c \/) (if (= c j) (recur (next (next stream)) (str re c) curly-depth)
                    (recur (next stream) (str re (if (= \. j) c "/(?=[^\\.])"))
                         curly-depth))
         (= c \*) (recur (next stream) (str re "[^/]*") curly-depth)
         (= c \?) (recur (next stream) (str re "[^/]") curly-depth)
         (= c \{) (recur (next stream) (str re \() (inc curly-depth))
         (= c \}) (recur (next stream) (str re \)) (dec curly-depth))
         (and (= c \,) (< 0 curly-depth)) (recur (next stream) (str re \|)
                                                 curly-depth)
         (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur (next stream) (str re \\ c)
                                                  curly-depth)
         :else (recur (next stream) (str re c) curly-depth)))))

(defn glob-match? [glob s]
  (let [regex (if (= (first glob) \/)
                (re-pattern (str (glob-to-regex-str (.substring glob 1)) ".*"))
                (re-pattern (str ".*" (glob-to-regex-str glob) ".*")))]
    (if (re-matches regex s) true false)))
