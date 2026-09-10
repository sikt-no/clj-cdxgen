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

(ns cdxgen.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as jio]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.deps :as deps]
            [cheshire.core :as json]
            [clojure.tools.deps.util.maven :as maven]
            [clojure.tools.deps.util.session :as session]
            [clj-commons.digest :as clj-commons-digest]
            [clojure.tools.deps.extensions :as ext]
            [cdxgen.progress-bar :as pg]
            [babashka.process :as process])
  (:refer-clojure :exclude [name type ref])
  (:import
    (clojure.lang Symbol)
    [java.io File]

    ;; maven-resolver-api
    (java.net URLEncoder)
    (java.nio.charset StandardCharsets)
    (java.nio.file Files)
    (java.nio.file.attribute FileAttribute)
    (java.time Instant ZoneOffset ZonedDateTime)
    (java.time.format DateTimeFormatter)
    (java.time.temporal ChronoUnit)
    (java.util Locale)
    [org.eclipse.aether DefaultRepositorySystemSession RepositorySystem RepositorySystemSession]
    (org.eclipse.aether.repository RemoteRepository)
    [org.eclipse.aether.resolution ArtifactRequest ArtifactDescriptorRequest VersionRangeRequest
                                   VersionRequest ArtifactResolutionException ArtifactDescriptorResult]
    (org.eclipse.aether.transfer TransferEvent TransferListener)
    [org.eclipse.aether.version Version]

    ;; maven-resolver-util
    [org.eclipse.aether.util.version GenericVersionScheme]
    [org.apache.maven.settings Settings]))

(set! *warn-on-reflection* true)

(defonce ^:private nl (System/getProperty "line.separator"))

(defn- printerrln
  "println to *err*"
  [& msgs]
  (binding [*out* *err*
            *print-readably* nil]
    (pr (str (str/join " " msgs) nl))
    (flush)))

(defn- printerr
  "print to *err*"
  [& msgs]
  (binding [*out* *err*
            *print-readably* nil]
    (pr (str (str/join " " msgs)))
    (flush)))

(defn git-remote-origin [wd]
  (try
    (assert (string? wd))
    (let [v (process/shell {:dir wd :out :string} "git" "config" "--get" "remote.origin.url")
          v' (str/trim (:out v))
          v'' (if (str/starts-with? v' "ssh://")
                (subs v' (count "ssh://"))
                v')]
      ;(println "remote origin is:" v'' "for" wd)
      v'')
    (catch Exception _
      "unknown-origin")))


(defn git-head-sha [wd]
  (try
    (assert (string? wd))
    (let [v (process/shell {:dir wd :out :string} "git" "rev-list" "-1" "HEAD")
          v' (str/trim (:out v))]
      v')
    (catch Exception _
      "unknown-sha")))

(defn get-master-edn [deps-edn]
  (let [{:keys [root-edn user-edn project-edn]} (deps/find-edn-maps deps-edn)
        master-edn (deps/merge-edns [root-edn user-edn project-edn])
        master-edn' (merge {:mvn/repos {"central" {:url "https://repo1.maven.org/maven2/"}
                                        "clojars" {:url "https://repo.clojars.org/"}}}
                           master-edn)]
    master-edn'))

(defn- get-libs [master-edn aliases]
  (let [combined-aliases (clojure.tools.deps.edn/combine-aliases master-edn aliases)
        basis (session/with-session
                (deps/calc-basis master-edn {:resolve-args   (merge combined-aliases {:trace true})
                                             :classpath-args combined-aliases}))]
    (:libs basis)))

(defn- check-version
  [lib {:keys [mvn/version]}]
  (when (nil? version)
    (throw (ex-info (str "No :mvn/version specified for " lib) {}))))

(defn- get-classifier
  [lib coord]
  (check-version lib coord)
  (when (not= "" (.getClassifier (maven/coord->artifact lib coord)))
    (.getClassifier (maven/coord->artifact lib coord))))

(defn- get-group-id
  [lib coord]
  (check-version lib coord)
  (.getGroupId (maven/coord->artifact lib coord)))

(defn- get-artifact-id
  [lib coord]
  (check-version lib coord)
  (.getArtifactId (maven/coord->artifact lib coord)))

(defn- get-artifact
  [lib coord ^RepositorySystem system ^RepositorySystemSession session mvn-repos]
  (check-version lib coord)
  (try
    #_(when (not= "" (.getClassifier (maven/coord->artifact lib coord)))
        (println (.getClassifier (maven/coord->artifact lib coord))))
    (let [artifact (maven/coord->artifact lib coord)
          req (ArtifactRequest. artifact mvn-repos nil)
          result (.resolveArtifact system session req)
          repository (.getRepository result)
          repo-id (.getId repository)
          repo-url (if (instance? RemoteRepository repository)
                     (.getUrl ^RemoteRepository repository)
                     nil #_(println "repo of type" (type repository)))]
      (cond
        (.isResolved result)
        (do
          #_(let [aresult (.getArtifact result)]
              (println (.getExtension aresult)))
          {:mvn/repo-id  repo-id
           :mvn/repo-url repo-url
           :coord-paths  (ext/coord-paths lib coord :mvn mvn-repos)
           :cdx/sha-256  (let [v (ext/coord-paths lib coord :mvn mvn-repos)
                               sha (clj-commons-digest/sha-256 ^File (jio/file (first v)))]
                           sha)})
        (.isMissing result) (throw (ex-info (str "Unable to download: [" lib (pr-str (:mvn/version coord)) "]") {:lib lib :coord coord}))
        :else (throw (first (.getExceptions result)))))
    (catch ArtifactResolutionException e
      (throw (ex-info (.getMessage e) {:lib lib, :coord coord})))))

#_(def ^TransferListener console-listener
    (reify TransferListener
      (transferStarted [_ event]
        (let [event ^TransferEvent event
              resource (.getResource event)
              name (.getResourceName resource)
              repo (.getRepositoryId resource)]
          (printerrln "Downloading:" name "from" repo)))
      (transferCorrupted [_ event]
        (printerrln "Download corrupted:" (.. ^TransferEvent event getException getMessage)))
      (transferFailed [_ event]
        ;; This happens when Maven can't find an artifact in a particular repo
        ;; (but still may find it in a different repo), ie this is a common event
        #_(printerrln "Download failed:" (.. ^TransferEvent event getException getMessage)))
      (transferInitiated [_ _event])
      (transferProgressed [_ _event])
      (transferSucceeded [_ _event])))

(def ^TransferListener silent-listener
  (reify TransferListener
    (transferStarted [_ event]
      (let [event ^TransferEvent event
            resource (.getResource event)
            name (.getResourceName resource)
            repo (.getRepositoryId resource)]
        #_(print ".")
        #_(flush)
        #_(printerrln (.getName (Thread/currentThread)) "Downloading:" name "from" repo)))
    (transferCorrupted [_ event]
      (printerrln "Download corrupted:" (.. ^TransferEvent event getException getMessage)))
    (transferFailed [_ event]
      ;; This happens when Maven can't find an artifact in a particular repo
      ;; (but still may find it in a different repo), ie this is a common event
      #_(printerrln "Download failed:" (.. ^TransferEvent event getException getMessage)))
    (transferInitiated [_ _event])
    (transferProgressed [_ _event])
    (transferSucceeded [_ _event])))

(defn thousand-separator
  [x]
  (-> (String/format Locale/GERMANY "%,d" (to-array [x]))
      (str/replace "." "_")))

(defn ^TransferListener fish-listener [total-downloads]
  (let [seed (rand-int 1000000)]
    (reify TransferListener
      (transferStarted [_ event]
        (let [event ^TransferEvent event
              resource (.getResource event)
              name (.getResourceName resource)
              repo (.getRepositoryId resource)
              cnt (swap! total-downloads inc)]
          (printerr (pg/marine-line-len-seed
                      pg/theme-marine
                      88 #_(^[int int] Math/min 88 (swap! cnt inc))
                      0.35
                      (- seed cnt)
                      true)
                    "\r")

          #_(print ".")
          #_(flush)
          #_(printerrln (.getName (Thread/currentThread)) "Downloading:" name "from" repo)))
      (transferCorrupted [_ event]
        (printerrln "\nDownload corrupted:" (.. ^TransferEvent event getException getMessage)))
      (transferFailed [_ event]
        ;; This happens when Maven can't find an artifact in a particular repo
        ;; (but still may find it in a different repo), ie this is a common event
        #_(printerrln "Download failed:" (.. ^TransferEvent event getException getMessage)))
      (transferInitiated [_ _event])
      (transferProgressed [_ _event])
      (transferSucceeded [_ _event]))))

(defn ^TransferListener silent-listener [total-downloads]
  (reify TransferListener
    (transferStarted [_ event]
      (let [event ^TransferEvent event
            resource (.getResource event)
            name (.getResourceName resource)
            repo (.getRepositoryId resource)
            _cnt (swap! total-downloads inc)]
        #_(printerr (pg/marine-line-len-seed
                      pg/theme-marine
                      88 #_(^[int int] Math/min 88 (swap! cnt inc))
                      0.35
                      (- seed cnt)
                      true)
                    "\r")

        #_(print ".")
        #_(flush)
        #_(printerrln (.getName (Thread/currentThread)) "Downloading:" name "from" repo)))
    (transferCorrupted [_ event]
      (printerrln "\nDownload corrupted:" (.. ^TransferEvent event getException getMessage)))
    (transferFailed [_ event]
      ;; This happens when Maven can't find an artifact in a particular repo
      ;; (but still may find it in a different repo), ie this is a common event
      #_(printerrln "Download failed:" (.. ^TransferEvent event getException getMessage)))
    (transferInitiated [_ _event])
    (transferProgressed [_ _event])
    (transferSucceeded [_ _event])))

(defn- get-artifact-info
  [listener lib {:keys [extension] :or {extension "jar"} :as coord} _manifest {:keys [mvn/repos mvn/local-repo]}]
  (check-version lib coord)
  (when (contains? #{"jar"} extension)
    (let [local-repo (or local-repo @maven/cached-local-repo)
          system ^RepositorySystem (session/retrieve-local :mvn/system #(maven/make-system))
          settings ^Settings (session/retrieve :mvn/settings #(maven/get-settings))
          session ^RepositorySystemSession (session/retrieve-local :mvn/session #(maven/make-session system settings local-repo))
          _ (when listener
              (.setTransferListener ^DefaultRepositorySystemSession session listener))
          mvn-repos (maven/remote-repos repos settings)]
      (get-artifact lib coord system session mvn-repos))))

(defn delete-recursively [fname]
  (let [func (fn [func ^File f]
               (when (.isDirectory f)
                 (doseq [f2 (.listFiles f)]
                   (func func f2)))
               (try
                 (jio/delete-file f)
                 (catch Exception e
                   (printerrln "Could not delete file:" f)
                   (printerrln "Error message:" (ex-message e)))))]
    (func func (jio/file fname))))

(defonce
  temp-dir
  (delay
    (let [v (.getAbsolutePath (.toFile
                                (Files/createTempDirectory
                                  "cdxgen-m2"
                                  (into-array FileAttribute []))))]
      (.addShutdownHook (Runtime/getRuntime)
                        (Thread.
                          (fn []
                            #_(println "shutdown hook running")
                            (when (.exists (jio/file v))
                              (delete-recursively v)))))
      v)))

(defonce cache (atom {}))

(defn- mvn-artifact-info [progress master-edn artifact coord]
  (if (not= :mvn (:deps/manifest coord))
    (do
      (printerrln "Unsupported manifest type" (:deps/manifest coord))
      nil)
    (let [cache-key [artifact coord]]
      (if (contains? @cache cache-key)
        (get @cache cache-key)
        (let [empty-repo-path @temp-dir
              _ (delete-recursively empty-repo-path)
              master-edn' (assoc master-edn
                            :mvn/local-repo
                            empty-repo-path)
              artifact-info'' (session/with-session
                                (get-artifact-info
                                  progress
                                  artifact
                                  coord
                                  :mvn
                                  master-edn'))]
          (if-not (contains? (get master-edn :mvn/repos) (:mvn/repo-id artifact-info''))
            (do
              (printerrln "Unknown repo-id:" (pr-str (:mvn/repo-id artifact-info'')) "for artifact" artifact)
              (throw (ex-info (str "Unknown repo-id: "
                                   (pr-str (:mvn/repo-id artifact-info'')))
                              {})))
            (let [v (dissoc artifact-info'' :coord-paths)]
              (swap! cache assoc cache-key v)
              v)))))))


; "group": "org.clojure",
; "name": "core.memoize",
; "version": "1.0.253",
; "purl": "pkg:clojars/org.clojure/core.memoize@1.0.253",
; "type": "library",
; "bom-ref": "pkg:clojars/org.clojure/core.memoize@1.0.253")
;}

; "bom-ref" : "pkg:maven/io.netty/netty-transport-native-epoll@4.1.130.Final?classifier=linux-x86_64&type=jar",
; "purl" : "pkg:maven/io.netty/netty-transport-native-epoll@4.1.130.Final?classifier=linux-x86_64&type=jar",

(defn rewrite-local-git-submodule [lib coord]
  (when (and (= :deps (:deps/manifest coord))
             (contains? coord :local/root)
             (let [local-root (:local/root coord)]
               (.exists (jio/file local-root))
               (.exists (jio/file local-root ".git"))))
    (into (sorted-map)
          (-> coord
              (dissoc :exclusions)
              (dissoc :paths)
              (dissoc :deps/root)
              (dissoc :local/root)
              (assoc :git/url (git-remote-origin (:local/root coord)))
              (assoc :git/sha (git-head-sha (:local/root coord)))
              (assoc :lib lib)))))

; TODO this function does too much _and_ have a bad name
(defn coord-add-repo-id-classifier [listener master-edn lib coord]
  (cond
    (= :mvn (:deps/manifest coord))
    (let [v (into (sorted-map)
                  (-> coord
                      (dissoc :exclusions)
                      (dissoc :paths)
                      (assoc :lib lib)
                      (merge (mvn-artifact-info listener master-edn lib coord))
                      (assoc :mvn/classifier (get-classifier lib coord))))]
      v)

    (and (= :deps (:deps/manifest coord))
         (contains? coord :git/sha))
    (into (sorted-map)
          (-> coord
              (dissoc :exclusions)
              (dissoc :paths)
              (dissoc :deps/root)
              (assoc :lib lib)))

    ; rewrite git submodules that are using :local/root
    (and (= :deps (:deps/manifest coord))
         (contains? coord :local/root)
         (let [local-root (:local/root coord)]
           (.exists (jio/file local-root))
           (.exists (jio/file local-root ".git"))))
    (rewrite-local-git-submodule lib coord)

    ; rewrite :local/root that is inside this repository
    (and (= :deps (:deps/manifest coord))
         (contains? coord :local/root)
         (let [local-root (:local/root coord)]
           (.exists (jio/file local-root))
           (not (.exists (jio/file local-root ".git")))
           (.exists (jio/file local-root "deps.edn"))
           (let [cwd (.getAbsolutePath (jio/file "."))
                 cwd' (->> (seq cwd)
                           (drop-last 2)
                           (str/join ""))]
             (str/starts-with? local-root cwd'))))
    (let [cwd (.getAbsolutePath (jio/file "."))
          cwd' (->> (seq cwd)
                    (drop-last 2)
                    (str/join ""))
          new-local-root (subs (:local/root coord)
                               (inc (count cwd')))]
      (into (sorted-map)
            (-> coord
                (dissoc :exclusions)
                (dissoc :paths)
                (dissoc :deps/root)
                (assoc :local/root new-local-root)
                (assoc :lib lib))))

    ; :local/root that is outside this repository
    ;(and (= :deps (:deps/manifest coord))
    ;     (contains? coord :local/root)
    ;     (let [local-root (:local/root coord)]
    ;       (.exists (jio/file local-root))
    ;       (.exists (jio/file local-root "deps.edn"))))
    ;(into (sorted-map)
    ;      (-> coord
    ;          (dissoc :exclusions)
    ;          (dissoc :paths)
    ;          (dissoc :deps/root)
    ;          (assoc :lib lib))))

    (and (= :pom (:deps/manifest coord))
         (contains? coord :git/sha))
    (do
      (let [v (into (sorted-map)
                    (-> coord
                        (dissoc :exclusions)
                        (dissoc :paths)
                        (dissoc :deps/root)
                        (assoc :lib lib)))]
        v))

    (and (= :pom (:deps/manifest coord)))
    (do
      (printerrln coord)
      (throw (ex-info (str "Not implemented: " (pr-str (:deps/manifest coord)))
                      {:coord coord :lib lib})))

    :else
    (do
      (throw (ex-info (str "Not supported manifest type: " (pr-str (:deps/manifest coord)))
                      {:coord coord :lib lib})))))

(defn- url-encode [x]
  (URLEncoder/encode ^String x StandardCharsets/UTF_8))

(defn self-bom-component [{:keys [name bom-ref group purl version type]}]
  (array-map
    :bom-ref bom-ref
    :group group
    :name name
    :version version
    :purl purl
    :hashes [{:alg     "SHA-256"
              :content version}]
    :type type))

(defn self-bom-local-component [{:keys [bom-ref purl version]}
                                {:keys [^Symbol lib] :as coord}]
  (array-map
    :bom-ref (str bom-ref "#" lib)
    :group (.getNamespace lib)
    :name (.getName lib)
    :version version
    :purl (str purl "#" lib)
    :hashes [{:alg     "SHA-256"
              :content version}]
    :type "library"))

(defn coord-deps->bom-entry [coord]
  ; as per:
  ; https://github.com/package-url/purl-spec/blob/565d7f1d97b09d525b76d3cc5b62f521342284f1/types/github-definition.json
  (let [[group-id artifact classifier] (maven/lib->names (:lib coord))
        git-url (:git/url coord)
        _ (assert (nil? classifier))
        purl (str "pkg:generic/"
                  group-id
                  "/"
                  artifact
                  "@"
                  (or (:git/tag coord)
                      (:git/sha coord))
                  "?vcs_url="
                  (url-encode "git+")
                  (do
                    (assert (str/starts-with? git-url "https://"))
                    (let [git-url' (if (str/ends-with? git-url ".git")
                                     (subs git-url 0 (- (count git-url)
                                                        (count ".git")))
                                     git-url)]
                      git-url'))
                  (url-encode "@")
                  (:git/sha coord))
        version (:git/sha coord)]
    ; ^^ as per: https://github.com/package-url/purl-spec/blob/565d7f1d97b09d525b76d3cc5b62f521342284f1/types/generic-definition.json
    (array-map
      :bom-ref purl
      :group group-id
      :name artifact
      :version version
      :purl purl
      :hashes [{:alg     "SHA-1"
                :content (:git/sha coord)}]
      :type "library")))

(defn coord-deps-pom->bom-entry [coord]
  ; as per:
  ; https://github.com/package-url/purl-spec/blob/565d7f1d97b09d525b76d3cc5b62f521342284f1/types/generic-definition.json
  (let [[group-id artifact classifier] (maven/lib->names (:lib coord))
        git-url (:git/url coord)
        _ (assert (nil? classifier))
        purl (str "pkg:generic/"
                  group-id
                  "/"
                  artifact
                  "@"
                  (or (:git/tag coord)
                      (:git/sha coord))
                  "?vcs_url="
                  (do
                    (assert (str/starts-with? git-url "git@"))
                    (str
                      (url-encode "git@")
                      (subs git-url (count "git@"))))
                  (url-encode "@")
                  (:git/sha coord))
        version (:git/sha coord)]
    (array-map
      :bom-ref purl
      :group group-id
      :name artifact
      :version version
      :purl purl
      :hashes [{:alg     "SHA-1"
                :content (:git/sha coord)}]
      :type "library")))

(defn coord-mvn->bom-entry [coord]
  (let [lib (:lib coord)
        repo-id (:mvn/repo-id coord)
        group-id (str (get-group-id lib coord))
        artifact-id (str (get-artifact-id lib coord))
        classifier (:mvn/classifier coord)
        version (str (:mvn/version coord))
        repo-url (str (:mvn/repo-url coord))
        _ (assert (and (string? repo-url)
                       (not-empty (seq repo-url))))
        _ (assert (contains? coord :cdx/sha-256))
        purl (str "pkg:maven"
                  "/"
                  group-id
                  "/"
                  artifact-id
                  "@" version
                  "?"
                  (when classifier
                    (str "classifier=" classifier "&"))
                  "type=jar"
                  (when (not= repo-url "https://repo.maven.apache.org/maven2/")
                    (str "&repository_url="
                         #_(do
                             (println repo-url)
                             nil)
                         repo-url)))]
    ; ^^ as per: https://github.com/package-url/purl-spec/blob/main/types/maven-definition.json
    (array-map
      :bom-ref purl
      :group group-id
      :name artifact-id
      :version version
      :purl purl
      :hashes [{:alg     "SHA-256"
                :content (:cdx/sha-256 coord)}]
      :type "library")))


(defn coord->bom-entry [opts coord]
  (cond
    (= :mvn (:deps/manifest coord))
    (coord-mvn->bom-entry coord)

    (and (= :deps (:deps/manifest coord))
         (contains? coord :git/sha)
         (contains? coord :git/url)
         (str/starts-with? (:git/url coord) "git@"))
    (coord-deps-pom->bom-entry coord)

    (and (= :deps (:deps/manifest coord))
         (contains? coord :git/sha)
         (contains? coord :git/url))
    (coord-deps->bom-entry coord)

    (and (= :deps (:deps/manifest coord))
         (contains? coord :local/root))
    (self-bom-local-component opts coord)

    (and (= :pom (:deps/manifest coord))
         (contains? coord :git/sha))
    (coord-deps-pom->bom-entry coord)

    :else
    (throw (ex-info (str "not supported manifest: " (pr-str (:deps/manifest coord)))
                    {:coord coord}))))

(defn deps-edn->libs
  "Returns a vector of libraries. This does not include the root project.
   Example vector entries:

   ; Standard maven jar
   {:lib org.clojure/clojure,
    :parents #{[]},
    :cdx/sha-256 \"4b81e9ba6da38c45d9cc58023c674062b8c9f0714f33ff00ded22e6a949da177\",
    ^^ this is the sha-256 of the JAR
    :deps/manifest :mvn,
    :mvn/classifier nil,
    :mvn/repo-id \"central\",
    :mvn/repo-url \"https://repo1.maven.org/maven2/\",
    :mvn/version \"1.12.4\"}

   ; deps.edn git library:
   {:lib io.github.joakimen/fzf.clj,
    :parents #{[]},
    :deps/manifest :deps,
    :git/sha \"2063e0f6e1a7f78b5869ef1424e04e21ec46e1eb\",
    :git/url \"https://github.com/joakimen/fzf.clj.git\"}

   ; local/root that is a git repo / submodule
   {:lib local-root/git-repo,
    :parents #{[]},
    :deps/manifest :deps,
    :git/sha \"9ceaaf4a1566de3e4399f43ad26f17966b3937cb\",
    :git/url \"git@codeberg.org/ivarref/clj-cdxgen.git\"}

   ; local/root inside the current directory
   {:lib local-root/something,
    :parents #{[]},
    :deps/manifest :deps,
    :local/root \"test-resources/local-root-2\"}

   ; local/root outside the current directory
   ; Not supported!
    "
  [{:keys [deps-edn-file aliases listener]}]
  (assert (.exists (jio/file deps-edn-file)))
  (let [master-edn (get-master-edn deps-edn-file)
        v (->> (get-libs master-edn aliases)
               (filterv some?)
               (mapv (fn [[lib coord]]
                       (let [v (coord-add-repo-id-classifier listener master-edn lib coord)]
                         v)))
               (sort-by (fn [x] (:lib x)))
               (vec))]
    v))


(defn libs->bom-components [opts libs]
  (let [self-bom (self-bom-component opts)
        v (->> libs
               (mapv (fn [x] (coord->bom-entry opts x)))
               (mapv (fn [x] (dissoc x :dependents)))
               (sort-by (juxt :group :name :version :purl))
               (vec))]
    (into [self-bom] v)))

(defn libs->dependencies [{:keys [bom-ref] :as opts} libs]
  (let [libs-map (into (sorted-map) (mapv (fn [x]
                                            [(:lib x)
                                             x])
                                          libs))
        libs-map' (atom [])
        dependencies (atom [])
        root-depends-on (atom [])]
    (doseq [lib libs]
      (when-let [dependents (not-empty (:dependents lib))]
        #_(println (clojure.core/type dependents))
        (doseq [dependent dependents]
          (swap! libs-map' conj {(dissoc (get libs-map dependent) :dependents)
                                 [(dissoc lib :dependents)]}))))
    (doseq [lib libs]
      (when (= #{[]} (:parents lib))
        (swap! root-depends-on conj (:bom-ref (coord->bom-entry opts (get libs-map (:lib lib)))))))
    (swap! dependencies conj (array-map :ref bom-ref :dependsOn (vec (sort @root-depends-on))))
    (let [libs-map'' (->> @libs-map'
                          (reduce (partial merge-with into) {}))
          libs' (mapv (fn [x] (dissoc x :dependents)) libs)]
      (doseq [lib libs']
        (let [bom-ref' (:bom-ref (coord->bom-entry opts lib))
              dependents-bomref (->> (get libs-map'' lib)
                                     (mapv (fn [x] (:bom-ref (coord->bom-entry opts x))))
                                     (sort)
                                     (vec))]
          (swap! dependencies conj (array-map :ref bom-ref' :dependsOn dependents-bomref)))))
    (->> @dependencies
         (mapv (fn [{:keys [ref dependsOn]}]
                 (array-map :ref ref
                            :dependsOn (vec (distinct (sort dependsOn)))))))))

(defn directory? [^File f]
  (some-> f .isDirectory))

; /Users/ire/.gitlibs/libs/com.github.ivarref/finddep/eb873c7f7a8f6240b60651bbbb770e6d3173eeea
(defn system-classpath []
  (-> (System/getProperty "java.class.path")
      (str/split (re-pattern (System/getProperty "path.separator")))
      (->> (map jio/as-file)
           (filter directory?))))

(defn find-version [group artifact]
  (assert (string? artifact))
  (assert (string? group))
  (let [matches (atom [])]
    (doseq [cp-entry (system-classpath)]
      ;(println (.getName ^File cp-entry))
      (when (= "src" (.getName ^File cp-entry))
        (when (= (count "dfb30dd6605cb6c0efc275e1df1736f6e90d4d73")
                 (count (.getName ^File (.getParentFile ^File (.getAbsoluteFile ^File cp-entry)))))
          (let [artifact-dir (.getParentFile ^File (.getParentFile ^File (.getAbsoluteFile ^File cp-entry)))]
            (when (= artifact (.getName ^File artifact-dir))
              (when (= group (.getName ^File (.getParentFile ^File artifact-dir)))
                (swap! matches conj (.getName ^File (.getParentFile ^File (.getAbsoluteFile ^File cp-entry))))))))))
    (if (= 1 (count @matches))
      (first @matches)
      nil #_(throw (ex-info (str "Could not find version for: " group artifact)
                            {})))))

(defn now-epoch-seconds []
  (long
    (/ (.toEpochMilli (Instant/now))
       1000)))

(defn now-utc-str [epoch-seconds]
  (let [inst (Instant/ofEpochSecond epoch-seconds)]
    (-> (ZonedDateTime/ofInstant inst ZoneOffset/UTC)
        (.truncatedTo ChronoUnit/SECONDS)
        (.format DateTimeFormatter/ISO_INSTANT))))

;   "metadata": {
;    "timestamp": "TODO",
;    "tools": {
;      "components": [
;        {
;          "type": "library",
;          "group": "com.github.sikt-no",
;          "name": "clj-cdxgen",
;          "version": "TODO"
;        }
;      ]
;    },
;    "lifecycles": [
;      {
;        "phase": "build"
;      }
;    ]
;  }

(def not-empty-string (fn [x]
                        (assert (string? x))
                        (assert (not-empty (seq x)))
                        x))

(defn- bom-entries->sbom-json [{:keys [epoch-seconds group name purl type version tool-version pretty?]}
                               bom-entries
                               dependencies]
  (let [header (json/parse-string
                 (slurp (jio/resource "cdx-bomheader.json"))
                 keyword)
        ;"component": {
        ;      "group": "authenticator",
        ;      "name": "authenticator",
        ;      "version": "0.1.0",
        ;      "properties": [
        ;        {
        ;          "name": "SrcFile",
        ;          "value": "/Users/ire/code/raird/authenticator/pom.xml"
        ;        }
        ;      ],
        ;      "purl": "pkg:maven/authenticator/authenticator@0.1.0?type=jar",
        ;      "bom-ref": "pkg:maven/authenticator/authenticator@0.1.0?type=jar",
        ;      "type": "application"
        ;    },
        metadata (array-map
                   :timestamp (now-utc-str epoch-seconds)
                   :tools (array-map
                            :components
                            [(array-map
                               :type "library"
                               :group "com.github.sikt-no"
                               :name "clj-cdxgen"
                               :version (not-empty-string tool-version))])
                   :lifecycles [{:phase "build"}]
                   :component (array-map
                                :group (not-empty-string group)
                                :name (not-empty-string name)
                                :version (not-empty-string version)
                                :purl (not-empty-string purl)
                                :bom-ref (not-empty-string purl)
                                :type (not-empty-string type)))
        sbom (array-map
               :bomFormat (:bomFormat header)
               :specVersion (:specVersion header)
               :serialNumber (:serialNumber header)
               :version (:version header)
               :metadata metadata
               :components bom-entries
               :dependencies dependencies)
        json-out (json/generate-string sbom {:pretty pretty?})]
    json-out))

(defn add-name-to-purl [purl name]
  (let [parts (str/split purl #"/")
        third (nth parts 2)
        parts-at (str/split third #"@")]
    (str/join "/"
              (into [(first parts)
                     (str (second parts) "-" name)
                     (str/join "@"
                               (into [(str (first parts-at) "-" name)]
                                     (drop 1 parts-at)))]
                    (drop 3 parts)))))

#_(defn dev []
    (let [purl "pkg:generic/authenticator/authenticator@bee00c76fa69eccdd8653b916a5452bba719e78e?vcs_url=git@gitlab.sikt.no:raird/authenticator.git%40bee00c76fa69eccdd8653b916a5452bba719e78e"]
      (println (add-name-to-purl purl "build-alias"))))

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

(defn new-deps-edn-file [deps-edn alias alias-type]
  (let [deps-map (get-in deps-edn [:aliases alias alias-type])
        _ (assert (map? deps-map))
        deps-output (-> deps-edn
                        (assoc :deps deps-map)
                        (dissoc :paths))]
    (let [f (File/createTempFile "clj_cdxgen_deps_" ".edn")]
      (.deleteOnExit f)
      (spit (.getAbsolutePath f) deps-output)
      (.getAbsolutePath f))))

(defn deps-edn->bom-js-string-single-default-alias [{:keys [progress total-downloads] :as opts}]
  (let [org-count @total-downloads
        libs (deps-edn->libs opts)
        bom-components (libs->bom-components opts libs)
        ;_ (pprint/pprint bom-components)
        dependencies (libs->dependencies opts libs)
        js (bom-entries->sbom-json opts bom-components dependencies)]
    (when (and (pos-int? (- @total-downloads org-count)) (= progress :fish))
      (printerrln ""))
    js))

(defn deps-edn->bom-js-string [{:keys [aliases deps-edn-file progress total-downloads] :as opts}]
  (if (= [] aliases)
    (deps-edn->bom-js-string-single-default-alias opts)
    (let [org-count' @total-downloads
          deps-edn (edn/read-string (slurp deps-edn-file))
          aliases' (into [{:name "standard-alias" :deps-edn-file deps-edn-file :aliases []}]
                         (->> (sort aliases)
                              (mapcat (fn [alias]
                                        (if-let [alias-type' (get-alias-type deps-edn alias)]
                                          (let [name' (str (clojure.core/name alias) "-alias")]
                                            (cond
                                              (= alias-type' :extra-deps)
                                              [{:name          name'
                                                :deps-edn-file deps-edn-file
                                                :aliases       [alias]}]
                                              (= alias-type' :deps)
                                              [{:name          name'
                                                :deps-edn-file (new-deps-edn-file deps-edn alias alias-type')
                                                :aliases       []}]
                                              (= alias-type' :replace-deps)
                                              [{:name          name'
                                                :deps-edn-file (new-deps-edn-file deps-edn alias alias-type')
                                                :aliases       []}]))
                                          [])))
                              (vec)))
          root-component (array-map
                           :bom-ref (:bom-ref opts)
                           :group (:group opts)
                           :name (:name opts)
                           :version (:version opts)
                           :purl (:purl opts)
                           :hashes [{:alg     "SHA-256"
                                     :content (:version opts)}]
                           :type (:type opts))
          root-depends-on (atom [])
          all-components (atom [root-component])
          all-deps (atom [])]
      (doseq [alias-setup aliases']
        (let [name' (:name alias-setup)
              aliases'' (:aliases alias-setup)
              deps-edn-file' (:deps-edn-file alias-setup)
              opts' (assoc opts
                      :deps-edn-file deps-edn-file'
                      :aliases aliases''
                      :purl (add-name-to-purl (get opts :purl) name')
                      :group (str name' "-" (get opts :group))
                      :name (str name' "-" (get opts :name)))
              libs' (deps-edn->libs opts')
              update-bomref (fn [bom-ref]
                              (assert (string? bom-ref))
                              (str name' "-" bom-ref))
              bom-components' (->> (libs->bom-components opts' libs')
                                   (mapv (fn [component]
                                           (update component :bom-ref update-bomref))))
              self-component-bom-ref (:bom-ref (first bom-components'))
              update-dependency (fn [dep]
                                  (-> dep
                                      (update :ref update-bomref)
                                      (update :dependsOn (fn [dep']
                                                           (mapv update-bomref dep')))))
              dependencies' (->> (libs->dependencies opts libs')
                                 (mapv update-dependency))]
          (swap! root-depends-on conj self-component-bom-ref)
          (swap! all-components into bom-components')
          (swap! all-deps into dependencies')))
      (let [root-deps (array-map :ref (:bom-ref opts)
                                 :dependsOn @root-depends-on)
            all-deps' (into [root-deps] @all-deps)
            js (bom-entries->sbom-json opts @all-components all-deps')]
        (when (and (pos-int? (- @total-downloads org-count')) (= progress :fish))
          (printerrln ""))
        js))))

(def default-name (delay
                    "clj-cdxgen" #_(.getName (.getParentFile (.getAbsoluteFile (jio/file "."))))))

; "purl" : "pkg:maven/io.github.tonsky/clj-reload@1.0.0?type=jar&repository_url=https://repo.clojars.org/",
(defn default-self-purl [group-id artifact coord]
  (let [purl (str "pkg:maven/"
                  group-id
                  "/"
                  artifact
                  "@"
                  coord
                  "?type=jar&repository_url=https://repo.clojars.org/")]
    purl))


(defn all-valid-aliases [deps]
  (->> (get deps :aliases {})
       (keys)
       (mapv (fn [alias]
               (when (get-alias-type deps alias)
                 alias)))
       (filterv some?)
       (sort)
       (vec)))

(defn dev []
  (let [p "/Users/ire/code/raird/user-service/authenticator"]
    (println (git-remote-origin p)))
  #_(println (all-valid-aliases (edn/read-string (slurp "deps.edn")))))

(defn adapted-opts
  [{:keys [deps-edn-file out-file epoch-seconds group progress name version purl
           err-exit-fn type exclude-aliases]
    :as   opts}
   now-epoch-seconds]
  (let [deps-edn-file' (or deps-edn-file "deps.edn")
        err-exit-fn' (or err-exit-fn
                         (fn [_code] (throw (IllegalStateException. "Aborting"))))
        _ (when (not (.exists (jio/file deps-edn-file')))
            (printerrln "File " (pr-str deps-edn-file) "does not exist. Exiting!")
            (err-exit-fn' 1))
        exclude-aliases' (into #{} (or exclude-aliases []))
        total-downloads (atom 0)
        progress (cond
                   (not= nil (System/getenv "CDXGEN_FISH"))
                   :fish

                   (not= nil (System/getProperty "cdxgen.progress.silent"))
                   :silent

                   (= progress :fish)
                   :fish

                   :else
                   :default)
        opts' {:deps-edn-file   (not-empty-string deps-edn-file')
               :out-file        (not-empty-string (or out-file "sbom.json"))
               :epoch-seconds   (let [v (or epoch-seconds now-epoch-seconds)]
                                  (assert (pos-int? v))
                                  v)
               :group           (not-empty-string (or group @default-name))
               :name            (not-empty-string (or name @default-name))
               :version         (not-empty-string (or version (git-head-sha ".")))
               :type            (not-empty-string (or type "application"))
               :tool-version    (if-let [v (find-version "com.github.sikt-no" "clj-cdxgen")]
                                  v
                                  "unknown")
               :pretty?         (get opts :pretty? true)
               :listener        (case progress
                                  :fish (fish-listener total-downloads)
                                  :silent (silent-listener total-downloads)
                                  nil)
               :progress        progress
               :total-downloads total-downloads
               :aliases         (let [aliases' (if (contains? opts :aliases)
                                                 (get opts :aliases)
                                                 (all-valid-aliases (edn/read-string (slurp deps-edn-file'))))]
                                  (vec (sort (into [] (set/difference (into #{} aliases')
                                                                      exclude-aliases')))))}
        purl' (not-empty-string
                (or purl
                    (default-self-purl (:group opts')
                                       (:name opts')
                                       "0.1.11" ;; patch-self-version
                                       #_asdf)))
        opts'' (assoc opts' :purl purl')]
    (assoc opts'' :bom-ref (get opts'' :bom-ref purl'))))


(defn write-sbom! [opts]
  (let [{:keys [out-file] :as opts'} (adapted-opts
                                       (assoc opts
                                         :err-exit-fn (fn [code] (System/exit code)))
                                       (now-epoch-seconds))
        js (deps-edn->bom-js-string opts')]
    (if (= "-" out-file)
      (println js)
      (do
        (spit out-file js)
        (printerrln "Wrote file" out-file)))))

#_(defn dev []
    #_(println (url-encode "https://janei.com"))
    (->> {:deps-edn-file "test-resources/deps-git.edn"}
         deps-edn->libs
         libs->bom-components
         (filterv (fn [{:keys [purl]}]
                    (or
                      (str/starts-with? purl "pkg:generic/")
                      (str/starts-with? purl "pkg:github/"))))
         bom-entries->sbom-json
         (println))
    #_(generate-sbom! {:deps-edn-file "test-resources/deps-test.edn"}))
