;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 2.0 (https://www.eclipse.org/legal/epl-2.0/)
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
;   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;
;   In addition, the following restrictions apply:
;
;   1. The Software and any modifications made to it may not be used for the purpose of training or improving machine learning algorithms,
;   including but not limited to artificial intelligence, natural language processing, or data mining. This condition applies to any derivatives,
;   modifications, or updates based on the Software code. Any usage of the Software in an AI-training dataset is considered a breach of this License.
;
;   2. The Software may not be included in any dataset used for training or improving machine learning algorithms,
;   including but not limited to artificial intelligence, natural language processing, or data mining.
;
;   3. Any person or organization found to be in violation of these restrictions will be subject to legal action and may be held liable
;   for any damages resulting from such use.
;
;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;   OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;
(ns cdxgen.rce-check
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.deps :as deps]
            [clojure.tools.deps.util.session :as session]
            [clojure.tools.gitlibs]
            [clojure.tools.deps.extensions.git]
            [babashka.process :as proc])
  (:import (java.io BufferedInputStream File FileInputStream)
           (java.nio.charset Charset StandardCharsets)
           (java.util.zip ZipEntry ZipInputStream)))


(def condo-cfg
  "{:linters {
              ;:single-operand-comparison {:level :error}\n
              :discouraged-var           {:level :error\n
                                          ;clojure.core/read-string {:message \"Use edn/read-string\"}\n
                                          clojure.core/eval       {:message \"Eval: Just dont do it\"}\n
                                          }
              }}\n")


(defn get-alias-type [deps-edn alias]
  (if-let [alias' (get-in deps-edn [:aliases alias])]
    (when (map? alias')
      (cond (contains? alias' :extra-deps)
            :extra-deps

            (contains? alias' :deps)
            :deps

            (contains? alias' :replace-deps)
            :replace-deps

            :else
            nil))
    nil))


(defn standalone-deps [deps-edn alias]
  (if-let [alias' (get-in deps-edn [:aliases alias])]
    (when (map? alias')
      (cond (contains? alias' :deps)
            (get alias' :deps)

            (contains? alias' :replace-deps)
            (get alias' :replace-deps)

            :else
            nil))
    nil))


(defn get-master-edn [fname]
  (let [{:keys [root-edn user-edn project-edn]} (deps/find-edn-maps fname)
        master-edn (deps/merge-edns [root-edn user-edn project-edn])]
    master-edn))


(defn get-libs
  ([aliases master-edn]
   (assert (vector? aliases))
   (let [standalone-deps' (atom [])
         extra-alias (atom [])]
     (doseq [alias aliases]
       (when (= :extra-deps (get-alias-type master-edn alias))
         (swap! extra-alias conj alias))
       (when-let [standalone-dep (standalone-deps master-edn alias)]
         (swap! standalone-deps' conj standalone-dep)))
     (if (> (count @standalone-deps') 1)
       (throw (ex-info "Must only specify a single standalone deps alias" {}))
       (if (= (count @standalone-deps') 0)
         (let [master-edn' (merge {:mvn/repos {"central" {:url "https://repo1.maven.org/maven2/"}
                                               "clojars" {:url "https://repo.clojars.org/"}}}
                                  master-edn)
               combined-aliases (deps/combine-aliases master-edn' @extra-alias)
               basis (session/with-session
                       (deps/calc-basis master-edn' {:resolve-args   (merge combined-aliases {:trace true})
                                                     :classpath-args combined-aliases}))
               libs (:libs basis)
               libs' (reduce-kv
                       (fn [o k v]
                         (assoc o k
                                  (update v :dependents (fn [old]
                                                          (if (not-empty old)
                                                            (into (sorted-set) old)
                                                            (sorted-set))))))
                       (sorted-map)
                       libs)]
           libs')
         (do
           (assert (= 1 (count @standalone-deps')))
           (let [master-edn'' (merge {:mvn/repos {"central" {:url "https://repo1.maven.org/maven2/"}
                                                  "clojars" {:url "https://repo.clojars.org/"}}}
                                     (assoc master-edn :deps (first @standalone-deps')))
                 combined-aliases' (deps/combine-aliases master-edn'' [])
                 basis' (session/with-session
                          (deps/calc-basis master-edn'' {:resolve-args   (merge combined-aliases' {:trace true})
                                                         :classpath-args combined-aliases'}))
                 libs'' (:libs basis')
                 libs''' (reduce-kv
                           (fn [o k v]
                             (assoc o k
                                      (update v :dependents (fn [old]
                                                              (if (not-empty old)
                                                                (into (sorted-set) old)
                                                                (sorted-set))))))
                           (sorted-map)
                           libs'')]
             libs'''))))))
  ([aliases]
   (let [{:keys [root-edn user-edn project-edn]} (deps/find-edn-maps "deps.edn")
         master-edn (deps/merge-edns [root-edn user-edn project-edn])]
     (get-libs aliases master-edn))))


(defn all-valid-aliases [deps]
  (->> (get deps :aliases {})
       (keys)
       (mapv (fn [alias']
               (when (get-alias-type deps alias')
                 alias')))
       (filterv some?)
       (sort)
       (vec)))




(def indent "   ")


(def errors (atom {}))
(def error-libs (atom (sorted-set)))


(def error-count (atom 0))


(defn process-bytes [{:keys [clj-kondo-config verbose? fail-fast?]}
                     lib mvn-version jar-path filename all-bytes]
  (let [stdin (^[byte/1 Charset] String/new all-bytes StandardCharsets/UTF_8)
        lang (if (str/ends-with? filename ".cljc")
               "--lang cljc"
               "--lang clj")
        res @(proc/process {:in  stdin
                            :out :string
                            :err :string}
                           (str "clj-kondo --report-level error --fail-level error "
                                lang " "
                                "--config '^:replace" clj-kondo-config "'" " "
                                "--lint -"))]
    (when (not= 0 (:exit res))
      (do
        (swap! errors
               (fn [old-errors]
                 (assoc old-errors [lib mvn-version] true)))
        (swap! error-libs conj lib)
        (when (not= "" (:out res))
          (doseq [lin (str/split-lines (str/trim (:out res)))]
            (when (not= lin "")
              #_(println indent lin)
              (if (str/starts-with? lin "<stdin>:")
                (do
                  (println "Error in library" lib)
                  (println indent "JAR:" (str jar-path))
                  (println indent "File:" (str filename))
                  (swap! error-count inc)
                  (println indent (str "Line: " (subs lin (count "<stdin>:"))))
                  (let [loc-info (first (str/split lin #"\s+"))
                        loc-info2 (subs loc-info (count "<stdin>:"))
                        line-number (parse-long (first (str/split loc-info2 #":")))
                        start-line (max 1 (- line-number 10))
                        cmd (str "bat --language=clojure --force-colorization "
                                 "--highlight-line " line-number " "
                                 "--line-rarge " (max 1 (- line-number 30)) ":+30")
                        _res2 (when fail-fast?
                                (let [first-n-lines (->> (str/split-lines stdin)
                                                         (take (+ 20 start-line)))]
                                  @(proc/process {:in  (str/join "\n" first-n-lines)
                                                  :out :inherit
                                                  :err :inherit}
                                                 (str "bat --language=clojure --force-colorization"
                                                      " --highlight-line " line-number " "
                                                      " --line-range " start-line ":+20"))))]
                    (println indent (str "To have a further peek, execute:"))
                    (when true
                      (println indent "unzip -p"
                               (str "\""
                                    (str/replace jar-path
                                                 (System/getProperty "user.home")
                                                 "$HOME")
                                    "\"")
                               filename "\\")
                      (println indent "|" cmd))
                    #_(doseq [lin' (str/split-lines (:out res2))]
                        (let [lin'' (str/trim lin')]
                          (println "    " lin'')))))
                (println "unexpexted line:" lin)))))
        (when (not= "" (:err res))
          (println (str/trim (:err res))))))))


(defn check-jar [{:keys [verbose?] :as opts} lib mvn-version jar-path]
  #_(println "checking" jar-path "...")
  (with-open [fis (FileInputStream. (io/file jar-path))
              bis (BufferedInputStream. fis)
              zis (ZipInputStream. bis)]
    (loop []
      (when-let [^ZipEntry entry (.getNextEntry zis)]
        (let [nam (.getName entry)]
          (when (or (str/ends-with? nam ".clj")
                    (str/ends-with? nam ".cljc"))
            (process-bytes opts lib mvn-version jar-path nam (.readAllBytes zis))))
        (recur))))
  (if (nil? (get @errors [lib mvn-version]))
    (when verbose?
      #_(println "OK JAR:" jar-path))
    (when verbose?
      #_(println "ERROR in JAR:" jar-path))))


(defn new-deps-edn-file [deps-edn alias' alias-type]
  (let [deps-map (get-in deps-edn [:aliases alias' alias-type])
        _ (assert (map? deps-map))
        deps-output (-> deps-edn
                        (assoc :deps deps-map)
                        (dissoc :paths))]
    (let [f (File/createTempFile "rcecheck_deps_" ".edn")]
      (.deleteOnExit f)
      (spit (.getAbsolutePath f) deps-output)
      (.getAbsolutePath f))))


(defn alias->jars [deps-edn-file alias']
  (let [result (atom #{})
        deps-edn (edn/read-string (slurp deps-edn-file))]
    (when-let [alias-type' (get-alias-type deps-edn alias')]
      (let [{:keys [aliases deps-edn-file]}
            (cond (= alias-type' :extra-deps)
                  {:deps-edn-file "deps.edn"
                   :aliases       [alias']}
                  (= alias-type' :deps)
                  {:deps-edn-file (new-deps-edn-file deps-edn alias' alias-type')
                   :aliases       []}
                  (= alias-type' :replace-deps)
                  {:deps-edn-file (new-deps-edn-file deps-edn alias' alias-type')
                   :aliases       []})
            libs (get-libs aliases (get-master-edn deps-edn-file))
            libs' (reduce-kv (fn [o k v]
                               (assoc o k (-> v
                                              (dissoc :dependents)
                                              (dissoc :parents))))
                             (sorted-map)
                             libs)]
        (doseq [k (keys libs')]
          (let [v (get libs' k)]
            (when (and (contains? v :paths)
                       (vector? (get v :paths)))
              (when (= :mvn (:deps/manifest v))
                (doseq [path (get v :paths)]
                  (when (str/ends-with? path ".jar")
                    (swap! result conj [k (get v :mvn/version) path])))))))
        @result))))


(defn scan! [{:keys [verbose? focus clj-kondo-config fail-fast? whitelist] :as opts}]
  (let [aliases'' (all-valid-aliases (edn/read-string (slurp "deps.edn")))
        all-jars (atom (sorted-set))
        whitelist' (or whitelist #{})
        all-jars2 (atom (sorted-set))
        skip-count (atom 0)]
    (assert (set? whitelist'))
    (assert (string? clj-kondo-config))
    (doseq [alias' aliases'']
      (swap! all-jars set/union (alias->jars "deps.edn" alias')))
    (doseq [[lib mvn-version path] (into (sorted-set) @all-jars)]
      (assert (string? mvn-version))
      (when (contains? whitelist' [lib (str mvn-version)])
        nil #_(println "Skipping" lib mvn-version))
      (if (not= nil focus)
        (if (= focus lib)
          (swap! all-jars2 conj [lib mvn-version path])
          (swap! skip-count inc))
        (if (or (contains? whitelist' lib)
                (contains? whitelist' [lib (str mvn-version)])
                (and (not= nil focus)
                     (= focus lib)))
          (swap! skip-count inc)
          (swap! all-jars2 conj [lib mvn-version path])
          #_(println "skipping" lib))))
    (println "Found" (count @all-jars2) "JARs to analyze (skipping" @skip-count "JARs)")
    (doseq [[lib mvn-version path] (into (sorted-set) @all-jars2)]
      (check-jar opts lib mvn-version path)
      (when (not= 0 @error-count)
        (when fail-fast?
          (println "Failing fast! Error count:" @error-count "lib ="
                   (str "\n[" lib
                        " "
                        (str "\"" mvn-version "\"")
                        "]"))
          (System/exit 1))))
    (when (not= 0 @error-count)
      (println "Error libraries:")
      (doseq [lib @error-libs]
        (println indent lib)))
    (println "Processed" (count @all-jars2) "JARs."
             "Error count:" @error-count)))


(defn dev []
  (scan! nil))

;/Users/ire/.m2/repository/org/clojure/tools.gitlibs/2.6.217/tools.gitlibs-2.6.217.jar"))
