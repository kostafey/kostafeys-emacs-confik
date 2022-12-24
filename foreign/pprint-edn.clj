#!/usr/bin/env boot
(require '[clojure.edn :as edn]
         '[clojure.pprint :refer [pprint]])

(defn -main [& args] (pprint (edn/read *in*)))
