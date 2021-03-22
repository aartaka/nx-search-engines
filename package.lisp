;;;; package.lisp

(defpackage #:nx-search-engines
  (:nicknames #:search-engines #:engines)
  (:use #:cl)
  (:import-from #:nyxt
                #:define-class
                #:define-mode
                #:define-command
                #:search-engine)
  (:export #:duckduckgo
           #:duckduckgo-images
           #:google
           #:google-images
           #:bing-date
           #:bing
           #:bing-images
           #:bing-videos
           #:bing-maps
           #:bing-news
           #:bing-shopping
           #:wordnet)
  (:documentation "A collection of search engines for Nyxt browsers."))
