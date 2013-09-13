(ns feat_fettish.core 
  "Provides functions for aggregating wordcounts and performing
  feature analysis on posts"
  (:require [ feat_fettish.reddit-scraper :as scraper ]
            [ feat_fettish.word-count :as word-count ]
            [ feat_fettish.post-set :as ps ]
            [ feat_fettish.word-matrix :as wm ]
            [ feat_fettish.test-posts :as ts ]
            [ feat_fettish.display-server :as ds]))

(defn update-subreddit-features
  "Updates the featuers listing for the given subreddit with
  the features extracted from a sample of its top content.

  The given features may include the following options:
  - :n-posts the number of posts which should be sampled from the
    subreddits top content 
  - :use-test-posts will use test post set instead of actual content
    from the given subreddit if this is true
  - :n-features The number of features that should be abstracted from the
    resulting corpus.
  "
  [subreddit & opts]
  (let [opts (apply hash-map opts)
        n-posts (get opts :n-posts 10) 
        n-features (get opts :n-features 10) 
        test-posts (ts/make-test-posts)
        posts (if (get opts :use-test false)
                    (ts/make-test-posts)
                    (scraper/top-posts subreddit n-posts))
        post-set (ps/make-postset subreddit posts)
        _ (prn post-set)
        post-set* (wm/nmf-postset post-set :n-features n-features)]
    (send ds/computation-status (constantly post-set*))))
