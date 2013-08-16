(ns feat_fettish.tf-idf)

(defn- tf 
  [term post]
  (get (:word-counts post) term))

(defn- idf
  [term post-set]
  (Math/log (/ (count (:posts post-set))
               (get (:article-counts post-set) term ))))

(defn tf-idf
  "Returns a weight for the given for the given post within the given post-set
  according to the tf-idf algorithm"
  [term post post-set]
  (* (tf term post) (idf term post-set)))
