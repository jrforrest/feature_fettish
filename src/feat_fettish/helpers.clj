(ns feat_fettish.helpers
  (:require [taoensso.carmine :as redis]))

(defmacro redis-do
  [& body] `(redis/wcar {:pool {} :spec {}} ~@body))
