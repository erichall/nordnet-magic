(ns nordnet-magic.main
  (:require [nordnet-magic.core :refer [init!]]
            [clojure.java.io :as io])
  (:gen-class))

(defn exists?
  [file]
  (.exists (io/as-file file)))

(defn print-exit
  [msg]
  (println msg)
  (System/exit 0))

(defn -main
  [& args]
  (let [conf (apply hash-map args)
        config-file (get conf "--config")]
    (if (and (nil? config-file) (not (exists? config-file)))
      (print-exit (str "Unable to find config-file: " config-file " provide it with --config <file-name>.edn"))
      (let [conf (-> (slurp config-file) clojure.edn/read-string)]
        (init! conf)))))

