(defproject rs "0.1.0-SNAPSHOT"
  :description "reasoned schemer"
  :url "https://github.com/ananthakumaran/reasoned-schemer"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.logic "1.0.0"]]
  :repl-options {:init-ns rs.core})
