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
(ns clj-cdxgen.basic-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clj-cdxgen.pretty-print :as pprint]
            [cdxgen.core :as cdx]))


(deftest aaa-self-url
  (is (= "pkg:maven/com.github.sikt-no/clj-cdxgen@0.0.1?type=jar&repository_url=https://repo.clojars.org/"
         (cdx/default-self-purl
           "com.github.sikt-no"
           "clj-cdxgen"
           "0.0.1"))))


(deftest basic
  (is (= (let [v (->> (cdx/deps-edn->libs {:deps-edn-file "test-resources/deps-test.edn"
                                           :version       "bd2e8246f5e042820ab26fa7bf9007e43dc79513"})
                      (mapv (fn [x] (-> x
                                        (dissoc :dependents)
                                        (dissoc :parents)))))]
           #_(mapv #(println (pr-str %)) v)
           #_(pprint/print-table-2 ;; must-disable
               [:lib :mvn/version :mvn/classifier :mvn/repo-id :mvn/repo-url :deps/manifest :cdx/sha-256]
               v
               {:to-file "test-resources/deps-test-libs.edn"})
           v)
         (edn/read-string (slurp "test-resources/deps-test-libs.edn")))))


(deftest bom
  (is (= (try
           (let [opts (cdx/adapted-opts {:version       "bd2e8246f5e042820ab26fa7bf9007e43dc79513"
                                         :purl          "my-pkg"
                                         :deps-edn-file "test-resources/deps-test.edn"} 123)
                 v (->> (cdx/deps-edn->libs opts)
                        (cdx/libs->bom-components opts))]
             #_(pprint/print-table-2 ;; must-disable
                 [:group :name :version :purl :bom-ref :hashes :type]
                 v
                 {:to-file "test-resources/components-bom.edn"})
             #_(mapv (comp println pr-str) v)
             v)
           (catch Exception e
             (println (ex-message e))
             nil))
         (edn/read-string (slurp "test-resources/components-bom.edn")))))


(deftest git
  (let [opts (cdx/adapted-opts {:version       "self-version"
                                :purl          "my-pkg"
                                :deps-edn-file "test-resources/deps-git.edn"} 123)
        v (->> (cdx/deps-edn->libs opts)
               (cdx/libs->bom-components opts))]
    #_(pprint/print-table-2 ;; must-disable
        [:group :name :version :purl :bom-ref :hashes :type]
        v
        {:to-file "test-resources/components-bom-git.edn"} #_{})
    (is (= (edn/read-string (slurp "test-resources/components-bom-git.edn"))
           v))))


(deftest git-purl
  (let [opts (cdx/adapted-opts {:deps-edn-file "test-resources/deps-git.edn"} 123)
        v (->> (cdx/deps-edn->libs opts)
               (cdx/libs->bom-components opts)
               (filterv (fn [{:keys [purl]}]
                          (str/starts-with? purl "pkg:generic/")))
               (vec))]
    ;(mapv (comp println pr-str) (mapv :purl v))
    (is (= ["pkg:generic/com.clojure/spec.alpha@a65fb3aceec67d1096105cab707e6ad7e5f063af?vcs_url=git%40github.com:clojure/spec.alpha.git%40a65fb3aceec67d1096105cab707e6ad7e5f063af"
            "pkg:generic/com.github.ivarref/finddep@0.1.77?vcs_url=git%2Bhttps://github.com/ivarref/finddep%40eb873c7f7a8f6240b60651bbbb770e6d3173eeea"
            "pkg:generic/io.github.cognitect-labs/test-runner@v0.5.1?vcs_url=git%2Bhttps://github.com/cognitect-labs/test-runner%40dfb30dd6605cb6c0efc275e1df1736f6e90d4d73"
            "pkg:generic/io.github.joakimen/fzf.clj@2063e0f6e1a7f78b5869ef1424e04e21ec46e1eb?vcs_url=git%2Bhttps://github.com/joakimen/fzf.clj%402063e0f6e1a7f78b5869ef1424e04e21ec46e1eb"
            "pkg:generic/org.codeberg.ivarref/cx@6e96cd2750603c61e0a36468dbf08feed4fbafe8?vcs_url=git%2Bhttps://codeberg.org/ivarref/cx%406e96cd2750603c61e0a36468dbf08feed4fbafe8"]
           (mapv :purl v)))))
;; ^^ as per https://github.com/package-url/purl-spec/blob/565d7f1d97b09d525b76d3cc5b62f521342284f1/types/generic-definition.json


(deftest maven-purl
  (let [opts (cdx/adapted-opts {:deps-edn-file "test-resources/deps-git.edn"} 123)
        v (->> (cdx/deps-edn->libs opts)
               (cdx/libs->bom-components opts)
               (filterv (fn [{:keys [purl]}] (str/starts-with? purl "pkg:maven/")))
               (drop 1)
               (take 5)
               (vec))]
    #_mapv (comp println pr-str) (mapv :purl v)
    (is (= ["pkg:maven/aopalliance/aopalliance@1.0?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/babashka/fs@0.4.19?type=jar&repository_url=https://repo.clojars.org/"
            "pkg:maven/babashka/process@0.6.25?type=jar&repository_url=https://repo.clojars.org/"
            "pkg:maven/clansi/clansi@1.0.0?type=jar&repository_url=https://repo.clojars.org/"
            "pkg:maven/com.cognitect/http-client@1.0.127?type=jar&repository_url=https://repo1.maven.org/maven2/"]
           (mapv :purl v)))))
;; ^^ as per https://github.com/package-url/purl-spec/blob/565d7f1d97b09d525b76d3cc5b62f521342284f1/types/maven-definition.json


(deftest maven-purl-classifier
  (let [opts (cdx/adapted-opts {:deps-edn-file "test-resources/deps-test.edn"} 123)
        v (->> (cdx/deps-edn->libs opts)
               (cdx/libs->bom-components opts)
               (filterv (fn [{:keys [purl]}]
                          (and (str/starts-with? purl "pkg:maven/")
                               (str/includes? purl "classifier"))))
               (take 5)
               (mapv :purl)
               (vec))]
    #_(mapv (comp println pr-str) v)
    (is (= v
           ["pkg:maven/io.netty/netty-transport-native-epoll@4.1.130.Final?classifier=linux-x86_64&type=jar&repository_url=https://repo1.maven.org/maven2/"]))))


(deftest local-ok
  (when-not (.exists (jio/file "local-root" ".git"))
    (jio/make-parents "local-root/.git/asdf"))
  (let [v (->> (cdx/deps-edn->libs {:deps-edn-file "test-resources/deps-local.edn"})
               (mapv (fn [x] (-> x
                                 (dissoc :dependents)
                                 (dissoc :parents)
                                 (update :mvn/classifier identity)
                                 (update :mvn/repo-id identity)
                                 (update :mvn/repo-url identity)
                                 (update :mvn/version identity)
                                 (update :cdx/sha-256 identity)
                                 (update :git/sha (fn [x]
                                                    (when (some? x)
                                                      "git-sha")))
                                 (update :git/url identity)))))]
    #_(pprint/print-table-2 ;; must-disable
        [:lib :git/sha :git/url :mvn/version :mvn/classifier :mvn/repo-id :mvn/repo-url
         :deps/manifest :cdx/sha-256]
        v
        {:to-file "test-resources/deps-local-libs.edn"})
    (is (= v
           (edn/read-string (slurp "test-resources/deps-local-libs.edn"))))))


(def sample-purl
  "pkg:generic/asdf/asdf@bee00c76fa69eccdd8653b916a5452bba719e78e?vcs_url=git@github.com:janei/janei.git%40bee00c76fa69eccdd8653b916a5452bba719e78e")


(deftest local-absolute-throws
  (jio/make-parents "/tmp/clj_cdxgen/deps.edn")
  (spit "/tmp/clj_cdxgen/deps.edn" "{:deps {com.github.ivarref/norwegian-national-id-validator {:mvn/version \"0.2.29\"}}}")
  (is (some?
        (try
          (let [opts (cdx/adapted-opts
                       {:deps-edn-file "test-resources/deps-absolute.edn"
                        :group         "org.codeberg"
                        :version       "bd2e8246f5e042820ab26fa7bf9007e43dc79513"
                        :purl          sample-purl}
                       1771008495)]
            (cdx/deps-edn->bom-js-string opts)
            nil)
          (catch Exception e
            e)))))


(deftest local-subfolder-ok
  (let [opts (cdx/adapted-opts
               {:deps-edn-file "test-resources/deps-local-subfolder.edn"
                :group         "org.codeberg"
                :version       "bd2e8246f5e042820ab26fa7bf9007e43dc79513"
                :purl          sample-purl}
               1771008495)]
    #_(cdx/deps-edn->bom-js-string opts)
    #_(spit "test-resources/sbom-deps-local-subfolder.json" ;; must-disable
            (cdx/deps-edn->bom-js-string opts))
    (is (= (json/parse-string (slurp "test-resources/sbom-deps-local-subfolder.json"))
           (json/parse-string (cdx/deps-edn->bom-js-string opts))))))


(deftest full-sbom
  (let [opts (cdx/adapted-opts
               {:deps-edn-file "test-resources/deps-test.edn"
                :group         "org.codeberg"
                :version       "bd2e8246f5e042820ab26fa7bf9007e43dc79513"
                :purl          sample-purl}
               1771008495)]
    #_(spit "test-resources/sbom-test.json" ;; must-disable
            (cdx/deps-edn->bom-js-string opts))
    (is (= (json/parse-string (slurp "test-resources/sbom-test.json"))
           (json/parse-string (cdx/deps-edn->bom-js-string opts))))))


(deftest full-sbom-2
  (let [opts (cdx/adapted-opts
               {:deps-edn-file "test-resources/deps-test-2.edn"
                :group         "org.codeberg"
                :version       "bd2e8246f5e042820ab26fa7bf9007e43dc79513"
                :purl          sample-purl}
               1771008495)]
    #_(spit "test-resources/sbom-test-2.json" ;; must-disable
            (cdx/deps-edn->bom-js-string opts))
    (is (= (json/parse-string (cdx/deps-edn->bom-js-string opts))
           (json/parse-string (cdx/deps-edn->bom-js-string opts))))
    (is (= (json/parse-string (slurp "test-resources/sbom-test-2.json"))
           (json/parse-string (cdx/deps-edn->bom-js-string opts))))))


(deftest full-sbom-aliases
  (let [opts (cdx/adapted-opts
               {:deps-edn-file "test-resources/deps-test.edn"
                :group         "org.codeberg"
                :version       "self-version"
                :purl          sample-purl
                :aliases       [:test]}
               1771008495)]
    #_(spit "test-resources/sbom-test-aliases.json" ;; must-disable
            (cdx/deps-edn->bom-js-string opts))
    #_(cdx/deps-edn->bom-js-string opts)
    (is (= (json/parse-string (slurp "test-resources/sbom-test-aliases.json"))
           (json/parse-string (cdx/deps-edn->bom-js-string opts))))))


(deftest full-sbom-aliases-build
  (let [opts (cdx/adapted-opts
               {:deps-edn-file "test-resources/deps-test.edn"
                :group         "org.codeberg"
                :version       "self-version"
                :purl          "pkg:generic/asdf/asdf@bee00c76fa69eccdd8653b916a5452bba719e78e?vcs_url=git@github.com:janei/janei.git%40bee00c76fa69eccdd8653b916a5452bba719e78e"
                :aliases       [:test :build]}
               1771008495)]
    #_(spit "test-resources/sbom-test-aliases-build.json" ;; must-disable
            (cdx/deps-edn->bom-js-string opts))
    #_(cdx/deps-edn->bom-js-string opts)
    (is (= (json/parse-string (slurp "test-resources/sbom-test-aliases-build.json"))
           (json/parse-string (cdx/deps-edn->bom-js-string opts))))))


(deftest dependencies
  (let [opts (cdx/adapted-opts {:deps-edn-file "test-resources/deps-dependencies-test.edn"} 123)
        v (->> (cdx/deps-edn->libs opts)
               (cdx/libs->dependencies opts)
               (filterv (fn [x]
                          (= (:ref x)
                             "pkg:maven/aleph/aleph@0.9.4?type=jar&repository_url=https://repo.clojars.org/")))
               (vec))]
    (is (= 1 (count v)))
    #_(mapv #(println (pr-str %)) (:dependsOn (first v)))
    (is (= (:dependsOn (first v))
           ["pkg:maven/io.netty.incubator/netty-incubator-transport-native-io_uring@0.0.26.Final?classifier=linux-aarch_64&type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty.incubator/netty-incubator-transport-native-io_uring@0.0.26.Final?classifier=linux-x86_64&type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-codec-http2@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-codec-http@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-codec@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-handler-proxy@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-handler@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-resolver-dns@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-resolver@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-transport-native-epoll@4.1.130.Final?classifier=linux-aarch_64&type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-transport-native-epoll@4.1.130.Final?classifier=linux-x86_64&type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-transport-native-kqueue@4.1.130.Final?classifier=osx-x86_64&type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-transport@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/manifold/manifold@0.5.0?type=jar&repository_url=https://repo.clojars.org/"
            "pkg:maven/metosin/malli@0.20.0?type=jar&repository_url=https://repo.clojars.org/"
            "pkg:maven/org.clj-commons/byte-streams@0.3.4?type=jar&repository_url=https://repo.clojars.org/"
            "pkg:maven/org.clj-commons/dirigiste@1.0.4?type=jar&repository_url=https://repo.clojars.org/"
            "pkg:maven/org.clj-commons/primitive-math@1.0.1?type=jar&repository_url=https://repo.clojars.org/"
            "pkg:maven/org.clojure/tools.logging@1.3.1?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/potemkin/potemkin@0.4.8?type=jar&repository_url=https://repo.clojars.org/"])))
  (let [opts' (cdx/adapted-opts {:deps-edn-file "test-resources/deps-dependencies-test.edn"} 123)
        v' (->> (cdx/deps-edn->libs opts')
                (cdx/libs->dependencies opts')
                (filterv (fn [x]
                           (= (:ref x)
                              "pkg:maven/io.netty/netty-codec@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/")))
                (vec))]
    (is (= 1 (count v')))
    #_(mapv #(println (pr-str %)) (:dependsOn (first v')))
    (is (= (:dependsOn (first v'))
           ["pkg:maven/io.netty/netty-buffer@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-common@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"
            "pkg:maven/io.netty/netty-transport@4.1.130.Final?type=jar&repository_url=https://repo1.maven.org/maven2/"]))))


(deftest self-dependencies
  (let [opts (cdx/adapted-opts {:version       "self-version"
                                :purl          "my-pkg"
                                :deps-edn-file "test-resources/deps-dependencies-test.edn"} 123)
        v (->> (cdx/deps-edn->libs opts)
               (cdx/libs->dependencies opts)
               (vec))]
    #_(println (pr-str (first v)))
    (is (= (first v)
           {:ref "my-pkg", :dependsOn ["pkg:maven/aleph/aleph@0.9.4?type=jar&repository_url=https://repo.clojars.org/"
                                       "pkg:maven/org.clojure/clojure@1.12.4?type=jar&repository_url=https://repo1.maven.org/maven2/"]}))
    #_(is (true? ()))
    #_(println (first v)))

  #_(let [v' (->> (cdx/deps-edn->libs {:deps-edn-file "test-resources/deps-dependencies-test.edn"})
                  cdx/libs->dependencies
                  (filterv (fn [x]
                             (= (:ref x)
                                "pkg:maven/io.netty/netty-codec@4.1.130.Final?type=jar")))
                  (vec))]
      (is (= 1 (count v')))
      (is (= (:dependsOn (first v'))
             ["pkg:maven/io.netty/netty-buffer@4.1.130.Final?type=jar"
              "pkg:maven/io.netty/netty-common@4.1.130.Final?type=jar"
              "pkg:maven/io.netty/netty-transport@4.1.130.Final?type=jar"]))))

#_(defn dev []
    (let [v (cdx/deps-edn->libs {:deps-edn-file "test-resources/deps-test.edn"})]
      #_(mapv #(println (pr-str %)) v)
      v)
    #_(pprint/print-table-2
        (edn/read-string (slurp "test-resources/components-bom-git.edn"))
        {}
        #_(vec (shuffle [{;:bom-ref "pkg:maven/io.github.tonsky/clj-reload@1.0.0?type=jar&repository_url=https%3A%2F%2Frepo.clojars.org",
                          :group   "io.github.tonsky",
                          :name    "clj-reload",
                          :purl    "pkg:maven/io.github.tonsky/clj-reload@1.0.0?type=jar&repository_url=https%3A%2F%2Frepo.clojars.org",
                          :type    "library",
                          :version "1.0.0"}
                         {;:bom-ref "pkg:maven/io.netty/netty-buffer@4.1.130.Final?type=jar",
                          :group   "io.netty",
                          :name    "netty-buffer",
                          :purl    "pkg:maven/io.netty/netty-buffer@4.1.130.Final?type=jar",
                          :type    "library",
                          :version "4.1.130.Final"}
                         {:group   "io.netty",
                          :name    "netty-transport-native-epoll",
                          :purl    "pkg:maven/io.netty/netty-transport-native-epoll@4.1.130.Final?classifier=linux-x86_64&type=jar",
                          :type    "library",
                          :version "4.1.130.Final"}]))))

#_(pprint/print-table)
