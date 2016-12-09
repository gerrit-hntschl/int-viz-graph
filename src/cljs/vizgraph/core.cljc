(ns vizgraph.core
  #?(:cljs (:require-macros [vizgraph.core :refer [fnk]]))
  (:require [plumbing.core :as plumbing]))

#?(:clj
   (defmacro fnk [bindings & body]
     `(vary-meta (plumbing/fnk ~bindings ~@body) assoc :source ~(apply str body))))


