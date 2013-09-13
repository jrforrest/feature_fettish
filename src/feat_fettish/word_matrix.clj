(ns feat_fettish.word-matrix
  "Proivides functions for creating and decomposing word matrixes for a
  post-set into feature sets.  These features can be used to identify like
  articles, and the word groups with which these articles were grouped.
  
  As non-negative matrix factorization does not require training with
  a pre-classified corpus, this method is best used where no training
  data is available."
  (:require [clatrix.core :as clatrix]))

(defn- word-weights
  "Returns a sequence of counts for the given set of words within
  the given post (using its :word-weights field)"
  [words {weights :word-weights}]
  (if (empty? weights)
    (throw (new Exception "Weights is empty")))
  (map #(get weights % 0) words))

(defn- form-matrix
  "Creates a data matrix for all of the given posts, using
  the each posts counts for the given words as data points.
  
  The data matrix is in the form of data X Posts, meaning
  that each row is a post and each column is a feature.

  In the following data matrix, article A mentions 'storm' 5 times.
  Article B; war 3 times.
  
  This diagram will be refered to as the example matrix elsewhere.

        storm  war  resonance 
      A   5     0       0
      B   0     3       0
      C   3     0       1
  "
  [words posts]
  (clatrix/matrix (map #(word-weights words %) posts)))

(defn- square
  "Squares the given number.
  
  Why the hell did I have to write this myself?"
  [x]
  (* x x))

(defn- diffcost
  "Calculates the divergence between the derived feature matrix 
  and the original data matrix
  
  This is determined by the sum of squares of the differences between
  the two given matrices."
  [data-matrix feature-matrix]
  (let [[dm-width dm-height] (clatrix/size data-matrix)
        difference-squares (for [i (range dm-width)]
                            (for [j (range dm-height)]
                              (square (- (clatrix/get data-matrix i j)
                                         (clatrix/get feature-matrix i j)))))]
    (reduce + 0 (flatten difference-squares))))

(defn- multiplicative-update
  "Recursively updates the given weights and features matrices
  to attempt to create an accurate decomposition of the given
  articles matrix.

  This function calls itself recursively according to the
  given number of iterations desired.
  
  When the divergence (as described by the diffcost function)
  is zero, the articles matrix is considered perfectly decomposed
  and the current weights and features matrices will be returned
  without further iterations.
  
  The update is performed via multiplicative update rules.  I'd be
  lying if I said I fully understand the linear algebra behind this algorithm.

  See http://sig.umd.edu/publications/Tjoa_ICASSP2_201003.pdf."
  [articles weights features iterations]
  (prn weights)
  (prn features)
  (let [synthetic-articles (clatrix/* weights features)
        cost (diffcost articles synthetic-articles)
        next-iter (- iterations 1)]
    (if (or (= cost 0) (< iterations 0))
      (list weights features)
      (let [
            transposed-weights (clatrix/t weights)
            hn (clatrix/* transposed-weights articles)
            hd (clatrix/* transposed-weights weights features)
            new-features (clatrix/div (clatrix/mult features hn) hd)
            transposed-features (clatrix/t features)
            wn (clatrix/* articles transposed-features)
            wd (clatrix/* weights features transposed-features)
            new-weights (clatrix/div (clatrix/mult weights wn) wd)]
        (recur articles new-weights new-features next-iter)))))

(defn- rand-matrix
  "Populate a matrix with random floats between 0 and 1"
  [m n]
  (clatrix/matrix (for [i (range m)]
                        (for [j (range n)]
                          (rand)))))

(defn- factorize-articles-matrix
  "Factors the given articles matrix, extracting the given number of features
  using the given number of iterations
  
  Random weights and features matrixes are created.  These matrixes may be
  multiplied together to attempt to recreate the original articles matrix.
  
  The features matrix contains a row for each feature, and a column for
  each word.  This maps each feature onto a weight for each word within
  the corpus.
  
  The weights matrix has a row for each article and a column for each
  post.  This maps each post onto a weight for each feature described
  in the features matrix.
  
  These two matrices are iteratively updated using multiplicative update
  rules, using the given number of iterations.  The fully updated weights and
  features matrixes are returned."
  [articles num-features iterations]
  (let [[dm-rows dm-cols] (clatrix/size articles)
        weights (rand-matrix dm-rows num-features)
        features (rand-matrix num-features dm-cols)]
    (multiplicative-update articles weights features iterations)))
    
(defn- all-valid-words
  "Because we're using TF-IDF to weight words now, some words will have
  a weight of 0 despite occuring in the all words list.  A word which
  occurs in the articles matrix but never has a score above 0 will cause
  issues in the NMF algorithm.

  This function should be used to generate a list of words from the given
  post-set which have at least one score above zero in the set
  
  note: This should probably get moved into the post-set ns"
  [{posts :posts}]
  (->> posts
       (map :word-weights)
       (reduce #(merge-with max %1 %2) (hash-map))
       (reduce
         (fn [valid-words word-weighting]
           (if (> (val word-weighting) 0)
             (conj valid-words (key word-weighting))
             valid-words))
         (hash-set))))

(defn- make-data-matrix
  "Creates a data matrix with all of the words in the given
  sequence of posts
  
  TODO: This should think the words out to a moderated set"
  [post-set]
  (let [words (all-valid-words post-set)]
    (form-matrix words (:posts post-set))))

(defn- feature-weights
  "Returns the given article row (from a weights matrix) as a
  sorted list of maps containing the feature number and score of
  weight"
  [feature-row]
  (sort 
    (comparator #(> (:score %) (:score %2)))
    (map (fn [i score] {:feature-number i, :score score})
         (range (count feature-row))
         feature-row)))

(defn- top-post-features
  "Identifies the top features for each post in the given weights matrix,
  and joins it with the corresponding (by sequential index) meta-data 
  in the given post-set"
  [weights post-set]
  (let [rows (clatrix/as-vec weights)
        articles-features (map feature-weights rows)]
    (map (fn [post article-features] (take 2 article-features))
         (:posts post-set)
         articles-features)))

(defn- top-feature-words
  "Identifies the top words for each feature in the given features
  matrix"
  [features post-set]
  (let [rows (clatrix/as-vec features)
        all-words (all-valid-words post-set)]
    (map (fn [row]
           (take 2 (sort
                     (comparator #(> (:weight %) (:weight %2)))
                     (map (fn [weight word] {:weight weight, :word word})
                          row all-words))))
         rows)))

(defn nmf-postset
  "Performs non-negative matrix factorization on the given post-set,
  returning the post-set.

  The :features field of the returned post-set will be set to a vector
  of the features identified within the post-set.  Each vector element will
  be a sorted map, with the top words to their weight in the feature, ordered
  from highest weighted word to least.
  
  The post hash-maps (within the :posts field of the returned post-set) will
  each contain a sorted :features map, which will contain feature ids (which
  correspond to the feature indexes in the :features vector) mapped to the
  weight with which that feature was weighted.  Only the top several features
  will be returned for each post, and they will be ordered by weight"
  [post-set & opts]
  (let [opts (apply hash-map opts)
        n-features (get opts :n-features 3)
        iterations (get opts :iterations 10)
        articles (make-data-matrix post-set)
        [weights features] (factorize-articles-matrix 
                             articles n-features iterations)]
    (-> post-set
        (assoc :features (top-feature-words features post-set))
        (assoc :posts (map #(assoc %1 :top-features %2 )
                           (:posts post-set)
                           (top-post-features weights post-set))))))
