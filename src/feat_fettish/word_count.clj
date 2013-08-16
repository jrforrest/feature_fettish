(ns feat_fettish.word-count
  "Analyzes word count stats from sequences of posts"
  (:require [ clojure.string :as string ]
            [ feat_fettish.dict-search :as dict ]
            [ clojure.string :as string ] ))

(defn- increment-wordcount
  "Increments the wordcount for word in the given hash-map of
  wordcounts"
  [wordcounts word]
  (let [cur-count (get wordcounts word 0)]
    (assoc wordcounts word (inc cur-count))))

(defn- normalize-word
  [word]
  word)
    
(defn- normalized-words
  "Normalizes the given words by lowercasing them and
  removing special characters"
  [body]
  (let [words (string/split body #"\s+")]
    (map
      #(-> % (.replaceAll "[^a-zA-Z0-9]" "") string/lower-case)
      words)))

(defn- valid-words
  "Filters any words from the given list which are 
  not in the dictionary"
  [words]
  (filter #(dict/in-dict? %)
          words))

(defn make-wordcounts
  "Given a sequence of words from a file, converts them into a
  normalized form and returns a map of words to the number of
  occurences within the set.  Words not in the dictionary file
  are removed."
  [words]
  (reduce
    (fn [wordcounts word]
      (increment-wordcount wordcounts (normalize-word word)))
    (hash-map)
    (->> words normalized-words valid-words)))


(defn total-wordcounts
  "Totals the word-counts in the given posts"
  [posts]
  (reduce 
    (fn [totals post] (merge-with + totals (:word-counts post)))
    (hash-map) 
    posts))

(defn word-article-counts
  "Determines the number of posts each word within the 
  set of posts occurs in"
  [posts]
  (let [word-occurances (fn [post]
                          (reduce (fn [counts [word _]] (assoc counts word 1))
                                  (hash-map)
                                  (:word-counts post)))
        word-counts (pmap word-occurances posts)]
    (reduce #(merge-with + %1 %2)
            (hash-map)
            word-counts)))

(defn- moderate-post-wordcounts
  "Given a post, total word counts for the post set and 
  article-counts for each word within the post set, returns the
  post with words not meeting the moderation criteria removed
  from its word counts."
  [post total-counts article-counts num-posts]
  (let [moderate-word? #(let [occurances (get article-counts % 0)]
                          (and (>= occurances 3) 
                               (< occurances (* num-posts 0.6))))
        moderated-wordcounts (into (hash-map)
                                  (filter #(moderate-word? (key %1)) 
                                          (:word-counts post)))]
        (assoc post :word-counts moderated-wordcounts)))

(defn moderate-wordcounts
  "In parallel, converts the word-counts of the given posts to
  only retain words which occur within all articles neither too 
  much or too little"
  [posts]
  (let [total-counts (future (total-wordcounts posts))
        article-counts (word-article-counts posts)]
    (pmap #(moderate-post-wordcounts % @total-counts 
                                     article-counts (count posts))
          posts)))
