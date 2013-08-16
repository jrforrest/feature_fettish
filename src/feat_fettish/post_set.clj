(ns feat_fettish.post-set
  "Provides logic for storing, retrieving and sequencing over
  sets of posts"
  (:require [feat_fettish.post :as post ]
            [feat_fettish.word-count :as wc ]
            [ taoensso.carmine :as redis]
            [feat_fettish.tf-idf :as tf])
  (:use [feat_fettish.helpers :only (redis-do)]))

(defn- recalc-counts
  "Recalculates the aggregate counts for the given word-set"
  [post-set]
  (-> post-set
      (assoc :total-wordcounts (wc/total-wordcounts(:posts post-set)))
      (assoc :article-counts (wc/word-article-counts (:posts post-set)))))

(defn- add-word-weights
  [post-set]
  "Adds a word-weights map to the given post-set, which maps each word
  from the word-counts keys to a weight for the word, generated using
  tf-idf"
  (let [make-word-weights (fn [post]
                            (reduce #(assoc %1 %2 (tf/tf-idf %2 post post-set))
                                    (hash-map) 
                                    (keys (:word-counts post))))
        add-weights (fn [post]
                      (assoc post :word-weights (make-word-weights post)))]
    (assoc post-set :posts (map add-weights (:posts post-set)))))

(defn- moderate-word-counts
  "Returns the given post set with the posts within modified by
  dropping words from their word-count sets which occur either too
  frequently or not frequently enough"
  [post-set]
  (assoc post-set :posts (wc/moderate-wordcounts (:posts post-set))))

(defn- remove-empty-posts
  "Removes any posts from the given post-set which have empty word-counts"
  [post-set]
  (let [posts* (remove #(empty? (:word-counts %)) (:posts post-set))]
    (assoc post-set :posts posts*)))

(defn make-postset
  "Makes a post set with the given name for the given sequence of posts.
  Excludes those posts with empty word-counts"
  [set-name posts]
  (let [pry (fn [x] (prn x) x)]
    (-> {:name set-name, :posts posts}
        recalc-counts
        moderate-word-counts
        remove-empty-posts
        recalc-counts
        add-word-weights
        pry)))

(defn add-post
  "Adds the given post to this post-set.
  
  This forces-recaculation of totals and article counts."
  [post-set post]
  (-> post-set
      (assoc :posts (cons post (:posts post-set)))
      recalc-counts))

(defn- fetch-urls
  "Fetches the url list for the given set-name from the datastore"
  [set-name]
  (redis-do (redis/smembers (str set-name ":urls"))))

(defn get-postset
  "Retrieves the post-set by the given name from the
  data store"
  [set-name]
  (let [urls (fetch-urls set-name)
        posts (map #(post/get-post %) urls)]
    (make-postset set-name posts)))

(defn save-postset
  "Saves the given post-set to the data store"
  [post-set]
  (redis-do
    (redis/set (str (:name post-set) ":total-wordcounts")
               (:total-wordcounts post-set))
    (redis/set (str (:name post-set) ":article-counts")
               (:article-counts post-set)))
  (doall 
    (map #(future
            (redis-do (redis/sadd 
                        (str (:name post-set) ":urls") 
                        (:url %)))
            (post/save-post %))
         (:posts post-set))))
