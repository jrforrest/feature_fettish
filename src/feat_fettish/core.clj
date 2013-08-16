(ns feat_fettish.core 
  "Provides functions for aggregating wordcounts and performing
  feature analysis on posts"
  (:require [ feat_fettish.reddit-scraper :as scraper ]
            [ feat_fettish.word-count :as word-count ]
            [ feat_fettish.post-set :as ps ]
            [ feat_fettish.word-matrix :as wm ]
            [ feat_fettish.test-posts :as ts ]
            [ feat_fettish.display-server :as ds]))

(defn subreddit-factorized
  [subreddit & opts]
  (let [opts (apply hash-map opts)
        n-posts (get opts :n-posts 5) 
        posts (take n-posts (scraper/subreddit-posts subreddit))
        test-posts (ts/make-test-posts)
        post-set (ps/make-postset subreddit posts)
        post-set (wm/nmf-postset post-set)]
    (send ds/computation-status (constantly post-set))))
