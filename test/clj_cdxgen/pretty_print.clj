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
(ns clj-cdxgen.pretty-print
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defonce ^:private nl (System/getProperty "line.separator"))

(defn- printerrln
  "println to *err*"
  [& msgs]
  (binding [*out* *err*
            *print-readably* nil]
    (pr (str (str/join " " msgs) nl))
    (flush)))

;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; this is the same as clojure.pprint/print-table
; but with some options I liked to have
; based on https://gist.github.com/benjamin-asdf/a15cfd8ac93bafc5affe18f9dc3662af

;(print-table-2
;    (files->rows (files "."))
;    {:left-aligned? {"Command" true}
;     :markdown?     true}

(defn- all-keys [rows]
  (let [ks (atom #{})]
    (doseq [row rows]
      (swap! ks set/union (into #{} (keys row))))
    (vec (sort (into [] @ks)))))

(defn print-table-2
  "Prints a collection of maps in a textual table. Prints table headings
   ks, and then a line of output for each row, corresponding to the keys
   in ks. If ks are not specified, use the keys of the first item in rows.
   opts:
  `:left-aligned?` if truthy, align the columns left instead of right.
  `:verify-keys?` if truthy, throw an exception if
  "
  {:added "1.3"}
  ([ks rows {:keys [to-file left-aligned? verify-item-missing-keys? verify-keys?]
             :or   {verify-keys?              true
                    verify-item-missing-keys? true}}]
   (when verify-keys?
     (let [all-keys-set (into (sorted-set) (all-keys rows))
           given-keys (into (sorted-set) ks)
           missing-keys (atom [])]
       (doseq [k all-keys-set]
         (when-not (contains? given-keys k)
           (swap! missing-keys conj k)
           (printerrln (str "Missing key: " k))))
       (when (not-empty @missing-keys)
         (throw (ex-info (str "missing keys: "
                              (str/join " " @missing-keys))
                         {:missing-keys @missing-keys})))))
   (when verify-item-missing-keys?
     (let [all-keys-set (into (sorted-set) (all-keys rows))
           error? (atom false)]
       (doseq [[idx row] (map-indexed vector rows)]
         (doseq [k all-keys-set]
           (when-not (contains? row k)
             (printerrln "Row with index" idx "missing key" (pr-str k) ". Value" (pr-str row))
             (reset! error? true))))
       (when @error?
         (throw (ex-info "One or more rows are missing keys" {})))))
   (when (seq rows)
     (let [widths (map
                    (fn [[idx k]]
                      (apply max (count (str k))
                             (map #(let [v (count (str (pr-str k) " " (pr-str (get % k))))]
                                     (if (= idx 0)
                                       (+ 2 v)
                                       v))
                                  rows)))
                    (map-indexed vector ks))
           fmts (map (fn [[idx width]]
                       (let [k (nth ks idx)]
                         (str "%" (when (get left-aligned? k true) "-") width "s")))
                     (map-indexed vector widths))
           fmt-row (fn [leader divider trailer idx row]
                     (str leader
                          (apply str (interpose
                                       divider
                                       (for [[col fmt] (map vector
                                                            (map (fn [[idx' k]]
                                                                   (str
                                                                     (if (and (= 0 idx)
                                                                              (= 0 idx'))
                                                                       "[{"
                                                                       (when (and (not= 0 idx)
                                                                                  (= 0 idx'))
                                                                         " {"))
                                                                     (pr-str k) " " (pr-str (get row k))
                                                                     (if (and (not= (count rows) (inc idx))
                                                                              (= (count ks) (inc idx')))
                                                                       "}"
                                                                       (when (and (= (count rows) (inc idx))
                                                                                  (= (count ks) (inc idx')))
                                                                         "}]"))))
                                                                 (map-indexed vector ks))
                                                            fmts)]
                                         (format fmt (str col)))))
                          trailer))]
       (if to-file
         (with-open [out-file (io/writer to-file :encoding "UTF-8")]
           (doseq [[idx row] (map-indexed vector rows)]
             (.write out-file (str (fmt-row "" "  " "" idx row)
                                   "\n"))))
         (doseq [[idx row] (map-indexed vector rows)]
           (println (fmt-row "" " " "" idx row)))))))
  ([rows opts] (print-table-2 (all-keys rows)
                              rows
                              opts)))