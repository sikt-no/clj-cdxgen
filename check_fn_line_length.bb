#!/usr/bin/env bb
(require '[clojure.edn :as edn]
         '[babashka.process :refer [shell]])

(def max-lines 70)

(let [{:keys [out]} (shell {:out :string :continue true}
                           "clj-kondo" "--lint" "src" "test"
                           "--config" "{:output {:format :edn} :analysis {:var-definitions true}}")
      defs (-> (edn/read-string out) :analysis :var-definitions)
      too-long (for [{:keys [filename ns name row end-row]} defs
                     :let [n (inc (- end-row row))]
                     :when (> n max-lines)]
                 (format "%s:%d: %s/%s is %d lines (max %d)" filename row ns name n max-lines))]
  (run! println too-long)
  (when (seq too-long) (System/exit 1)))
