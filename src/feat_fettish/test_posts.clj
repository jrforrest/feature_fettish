(ns feat_fettish.test-posts)

(defn make-test-posts
  []
  [{:title "Dog Food",
    :word-counts {"dog" 5, "cat" 1, "collie" 2, "food" 3, "the" 8}},
   {:title "Taking care of cats",
    :word-counts {"cat" 8, "food" 1, "mouse" 3, "blah" 2, "baz" 3}},
   {:title "Mouse Care",
    :word-counts {"mouse" 5, "food" 2, "cat" 1, "dslfkj" 6, "siw" 2}}
   {:title "Mouse Habits",
    :word-counts {"mouse" 4, "rabbit" 1, "si" 2, "the" 1, "a" 1}}
   {:title "Sled Dogs",
   :word-counts {"Alaska" 2, "dog" 6, "sled" 1, "nimrod" 2, "the" 5, "person" 1}}])
    
