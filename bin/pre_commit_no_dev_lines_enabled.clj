#!/usr/bin/env bb

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

; Installation:
; $ ln -sf "$(pwd)/bin/pre_commit_no_dev_lines_enabled.clj" .git/hooks/pre-commit

(ns pre-commit-no-dev-lines-enabled)

(require '[clojure.string :as string])
(require '[clojure.java.io :as io])
(require '[babashka.process :refer [shell]])

(defn println-err [s]
  (binding [*out* *err*]
    (println s)))

(let [files (-> (shell {:out :string} "find src test -name \"*.clj*\"")
                :out
                string/split-lines)
      error? (atom false)
      ok-disable-lines (atom 0)
      ok-enable-lines (atom 0)]
  (doseq [file files]
    (when (or (string/ends-with? file ".cljs")
              (string/ends-with? file ".cljc")
              (string/ends-with? file ".clj"))
      #_(let [ok? (atom true)])
      (with-open [reader (io/reader file)]
        (doseq [[idx line] (map-indexed vector (line-seq reader))]
          (let [line' (string/trim line)]
            (when (string/ends-with? line' "must-disable")
              (if (or
                    (string/starts-with? line' "#_")
                    (string/starts-with? line' ";"))
                (swap! ok-disable-lines inc)
                (do
                  (println-err (str file ":" (inc idx) " Error: This line must be disabled!"))
                  (reset! error? true)
                  #_(reset! ok? false))))
            (when (string/ends-with? line' "must-enable")
              (if (or
                    (string/starts-with? line' "#_")
                    (string/starts-with? line' ";"))
                (do
                  (reset! error? true)
                  #_(reset! ok? false)
                  (println-err (str file ":" (inc idx) " Error: This line must be enabled!")))
                (swap! ok-enable-lines inc)))))
        #_(when @ok?
            #_(println-err (str file " OK"))))))
  (if @error?
    (System/exit 1)
    (do
      (println-err (str @ok-disable-lines " OK must-disable lines."))
      (println-err (str @ok-enable-lines " OK must-enable lines.")))))

(defn run [_]
  nil)