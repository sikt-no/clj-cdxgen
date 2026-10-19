#!/usr/bin/env bash

SOURCE=${BASH_SOURCE[0]}
while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
  SOURCE=$(readlink "$SOURCE")
  [[ $SOURCE != /* ]] && SOURCE=$DIR/$SOURCE # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )

set -euo pipefail

clojure -Ttools install com.github.sikt-no/clj-cdxgen \
'{:local/root "'$DIR'"}' :as cdxgen-dev

clojure -Tcdxgen-dev cdxgen.rce-check/scan! "$@" \
:whitelist '#{
             clj-kondo/clj-kondo
             com.cognitect.aws/api
             lambdaisland/kaocha
             org.clojure/clojure
             org.clojure/core.rrb-vector
             org.clojure/data.priority-map
             [org.clojure/data.xml "0.2.0-alpha6"]
             [org.clojure/data.xml "0.2.0-alpha8"]
             [org.clojure/data.xml "0.2.0-alpha9"]
             [org.clojure/data.xml "0.2.0-alpha11"]
             org.clojure/spec.alpha
             org.clojure/tools.analyzer
             org.clojure/tools.analyzer.jvm
             org.clojure/tools.logging
             org.clojure/tools.reader
             [clj-http/clj-http "3.12.3"]
             [clj-http/clj-http "3.13.0"]
             [clojure-term-colors/clojure-term-colors "0.1.0"]
             [io.github.clojure/tools.build "0.10.14"]
             [io.github.clojure/tools.build "0.10.5"]
             [com.taoensso/encore "3.72.0"]
             [etaoin/etaoin "1.1.43"]
             [hiccup/hiccup "2.0.0"]
             [metosin/muuntaja "0.6.12"]
             [metosin/reitit-core "0.11.0"]
             [nrepl/nrepl "1.7.0"]
             [org.clj-commons/byte-streams "0.3.4"]
             [org.clj-commons/primitive-math "1.0.0"]
             [org.clojure/test.check "1.1.3"]
             [potemkin/potemkin "0.4.5"]
             [potemkin/potemkin "0.4.7"]
             [prismatic/schema "1.1.12"]
             [riddley/riddley "0.1.12"]
             [riddley/riddley "0.2.0"]
             [ring/ring-core "1.15.5"]
             [vvvvalvalval/scope-capture "0.3.3"]
             [zprint/zprint "1.3.0"]
             }' \
:fail-fast? true \
:clj-kondo-config '
  "{:linters {
              :single-operand-comparison {:level :error}\n
              :discouraged-var           {:level :error\n
                                          clojure.core/read-string {:message \"Use edn/read-string\"}\n
                                          clojure.core/eval       {:message \"Eval: Just dont do it\"}\n
                                          }
              }}\n"
'
