(ns feat_fettish.post
  "Handles logic around persisting and recalling a post"
  (:require [ taoensso.carmine :as redis]
            [ clojure.tools.logging :as log ]
            [ taoensso.carmine :as redis])
  (:use [feat_fettish.helpers :only (redis-do)]))

(defn get-post
  "Returns either the post belonging to the given url, or nil if it
  does not exist in the data store"
  [url]
  (log/info (str "Fetching from DS: " url))
  (let [title (redis-do (redis/get (str "posts:" url ":title")))
        word-counts (redis-do (redis/get (str "posts:" url ":word-counts")))]
    (if (or title word-counts)
      {:url url :title title :word-counts word-counts}
      nil)))

(defn save-post
  "Saves the given post to redis, with URL as the key"
  [post]
  (log/info (str "Saving Post: " (:url post)))
  (redis-do 
    (redis/sadd "posts" (:url post))
    (redis/set (str "posts:" (:url post) ":title")
               (:title post))
    (redis/set (str "posts:" (:url post) ":word-counts")
               (:word-counts post))))

(defn- persist-posts
  "Returns a sequence of futures mapping the given posts to
  the result of their persistence into the datastore
  
  TODO: Should probably be ditched in favor of just persisting post-sets"
  [posts]
  (let [post-futures (map #(future (save-post %)) posts)]
    (doall post-futures)
    post-futures))
