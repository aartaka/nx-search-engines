;;;; nx-search-engines.asd

(asdf:defsystem #:nx-search-engines
  :description "A collection of easy-to-setup search-engines for Nyxt browser."
  :author "Artyom Bologov"
  :license "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:nyxt)
  :components ((:file "package")
               (:file "nx-search-engines")
               (:file "search-engines-mode")))
