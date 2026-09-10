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
(ns build
  (:require [clojure.java.io :as io]
            [clojure.tools.build.api :as b]
            [clojure.java.shell :as shell]
            [clojure.edn :as edn]
            [clojure.string :as string])

  (:import java.time.format.DateTimeFormatter
           java.time.LocalDateTime))

(def changelog-file "CHANGELOG.md")
(def readme-file "README.md")
(def deps-file "deps.edn")
(def core-file "src/cdxgen/core.clj")
(def current-version
  (-> deps-file
      slurp
      edn/read-string
      :aliases
      :mvn/version))

(defn- valid-version?
  [new-ver old-ver]
  (let [[old-major old-minor old-patch] (map read-string (string/split old-ver #"\."))
        [new-major new-minor new-patch] (map read-string (string/split new-ver #"\."))]
    (and
      ;; Is new version in semver format?
      (first (re-matches #"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$"
                         new-ver))
      ;; Is new version newer than old version?
      (or (> new-major old-major)
          (and (>= new-major old-major)
               (> new-minor old-minor))
          (and
            (>= new-major old-major)
            (>= new-minor old-minor)
            (> new-patch old-patch))))))


(defn- today []
  (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "yyyy-MM-dd")))


(defn- bump-changelog
  [old-version new-version changelog]
  (->> changelog
       slurp
       string/split-lines
       (reduce
         #(str %1
               (cond
                 ; Update current block of changes
                 (string/includes? %2 "## [Unreleased]")
                 (str "## [Unreleased]\n\n## [" new-version "] - " (today))

                 ; Update compare tags
                 (string/includes? %2 "[Unreleased]:")
                 (str
                   (string/replace-first %2 old-version new-version)
                   "\n"
                   (-> %2
                       (string/replace-first "Unreleased" new-version)
                       (string/replace-first "main" new-version)))
                 :else %2)
               "\n")
         "")))


(defn- bump-readme
  [_old-version new-version readme-file]
  (->> readme-file
       slurp
       string/split-lines
       (reduce
         #(str %1
               (cond
                 (string/starts-with? %2 "{:deps    {com.github.sikt-no/clj-cdxgen {:mvn/version ")
                 (str "{:deps    {com.github.sikt-no/clj-cdxgen {:mvn/version \"" new-version "\"}}")

                 :else
                 %2)
               "\n")
         "")))


(defn- bump-deps-edn
  [_old-version new-version deps-file]
  (->> deps-file
       slurp
       string/split-lines
       (reduce
         #(str %1
               (cond
                 (string/starts-with? (string/trim %2) ":mvn/version ")
                 (string/replace %2 #"(\s+:mvn/version\s+\")([0-9.]+)" (str "$1" new-version))

                 :else
                 %2)
               "\n")
         "")))


(defn- bump-core-file
  [_old-version new-version core-file]
  (->> core-file
       slurp
       string/split-lines
       (reduce
         #(str %1
               (cond
                 (string/ends-with? (string/trim %2) " ;; patch-self-version")
                 (str "                                       \"" new-version "\" ;; patch-self-version")

                 :else
                 %2)
               "\n")
         "")))


(defn- update-changelog!
  [old-version new-version changelog-file]
  (spit changelog-file (bump-changelog old-version new-version changelog-file)))


(defn- update-readme!
  [old-version new-version readme-file]
  (spit readme-file (bump-readme old-version new-version readme-file)))


(defn- update-deps-edn!
  [old-version new-version deps-file]
  (spit deps-file (bump-deps-edn old-version new-version deps-file)))


(defn- update-core-file!
  [old-version new-version core-file]
  (spit core-file (bump-core-file old-version new-version core-file)))


(defn- next-patch [current-version]
  (-> current-version
      (string/split #"\.")
      (update 2 (comp inc read-string))
      (#(string/join "." %))))


(defn- next-minor [current-version]
  (-> current-version
      (string/split #"\.")
      (update 1 (comp inc read-string))
      (assoc 2 0)
      (#(string/join "." %))))


(defn- next-major [current-version]
  (-> current-version
      (string/split #"\.")
      (update 0 (comp inc read-string))
      (assoc 1 0)
      (assoc 2 0)
      (#(string/join "." %))))


(defn- error!
  [msg]
  (binding [*out* *err*]
    (println (str "Error: " msg)))
  (System/exit 1))


(defn- shell-cmd!
  [cmdv]
  (let [res (apply shell/sh cmdv)]
    (if (not= (:exit res) 0)
      (error! (str "Command '" (apply str (interpose " " cmdv)) "' failed with code " (:exit res) "; " (:err res)))
      res)))


(defn- git-commit-files-to-git!
  [files version]
  (println "Committing changes in" (apply str (concat (vec (interpose ", " (butlast files))) [" and " (last files)])) "to git")
  (shell-cmd! (concat ["git" "add"] files))
  (shell-cmd! ["git" "commit" "-m" (str "Bump version " version "")]))


(defn- git-tag-version!
  [version]
  (println "Adding git tag" version)
  (shell-cmd! ["git" "tag" "-a" version "-m" (str "Release " version " - " (today))]))


(defn- git-prepare
  [new-version]
  (if (= "main" (string/trim (:out (shell-cmd! ["git" "branch" "--show-current"]))))
    (do (println "Syncing tags with origin")
        (shell-cmd! ["git" "pull" "--tags" "origin"])
        (doseq [ver (string/split-lines (:out (shell-cmd! ["git" "tag" "-l"])))]
          (when (= new-version (string/trim ver))
            (error! "Version already exists in git tags. Giving up"))))
    (error! "Not on main branch, not tagging")))


(defn- update-version-files!
  [old-version new-version {:keys [changelog readme deps core-file]}]
  (update-changelog! old-version new-version changelog)
  (update-readme! old-version new-version readme)
  (update-deps-edn! old-version new-version deps)
  (update-core-file! old-version new-version core-file))


(defn- bump!
  [old-version new-version]
  (when (not (valid-version? new-version old-version))
    (error! (str new-version " is not a valid version")))
  (git-prepare new-version)
  (println "Bumping version" old-version "->" new-version)
  (update-version-files! old-version new-version {:changelog changelog-file
                                                  :readme    readme-file
                                                  :deps      deps-file
                                                  :core-file core-file})
  (git-commit-files-to-git! [deps-file readme-file changelog-file core-file] new-version)
  (git-tag-version! new-version))


(defn bump-patch
  "Bump the patch part of the version"
  [_opts]
  (bump! current-version (next-patch current-version)))


(defn bump
  "Alias for bump-patch"
  [_opts]
  (bump-patch nil))


(defn bump-minor
  "Bump the minor part of the version. Patch part is set to zero"
  [_opts]
  (bump! current-version (next-minor current-version)))


(defn bump-major
  "Bump the major part of the version. Other parts are set to zero"
  [_opts]
  (bump! current-version (next-major current-version)))


(defn set-version
  "Explicitly set version"
  [{:keys [version] :as params}]
  (bump! current-version version))

(defn add-license!
  [_]
  (let [new-lines (atom [])
        add-license (str
                      "  <scm>\n    <connection>scm:git:git://github.com/sikt-no/clj-cdxgen.git</connection>\n    <developerConnection>scm:git:ssh://git@github.com/sikt-no/clj-cdxgen.git</developerConnection>\n    <url>https://github.com/sikt-no/clj-cdxgen</url>\n  </scm>\n  <licenses>\n    <license>\n      <name>Eclipse Public License - v 2.0 NON-AI License</name>\n      <url>https://raw.githubusercontent.com/sikt-no/clj-cdxgen/refs/heads/main/LICENSE</url>\n    </license>\n  </licenses>")]
    (if (string/includes? (slurp "pom.xml")
                          add-license)
      (println "license etc up to date")
      (do
        (println "adding license ...")
        (with-open [reader (io/reader "pom.xml")]
          (doseq [[idx line] (map-indexed vector (line-seq reader))]
            (when (= idx 2)
              (doseq [extra-line (string/split-lines add-license)]
                (swap! new-lines conj extra-line)))
            (swap! new-lines conj line)))
        (spit "pom.xml" (string/join "\n" @new-lines))))))


(defn clean
  "Delete all build artifacts"
  [_]
  (b/delete {:path "pom.xml"})
  (b/delete {:path "target"}))