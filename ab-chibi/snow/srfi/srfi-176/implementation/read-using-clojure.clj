#!/usr/bin/env clojure

(defn read-version-alist-from-string [string]
  (let [string (str "\n" string)
        start  (or (clojure.string/index-of string "\n(") (count string))
        eof    (gensym)]
    (with-in-str (subs string start)
      (loop [xs {}]
        (let [x (clojure.edn/read {:eof eof} *in*)]
          (if (= eof x) xs (recur (conj xs [(first x) (rest x)]))))))))

(defn read-version-alist [in]
  (read-version-alist-from-string (slurp in)))

(print (read-version-alist *in*))
