;;;; package.lisp

(defpackage #:nx-search-engines
  (:nicknames #:search-engines #:engines)
  (:use #:cl)
  (:import-from #:nyxt
                #:define-class
                #:define-mode
                #:define-command
                #:current-mode
                #:search-engine
                #:new-url-query
                #:default-search-engine
                #:make-search-completion-function)
  (:import-from #:serapeum
                #:export-always)
  (:export #:make-duckduckgo-completion
           #:duckduckgo
           #:duckduckgo-images
           #:make-google-completion
           #:google
           #:google-images
           #:bing-date
           #:bing
           #:bing-images
           #:bing-videos
           #:bing-maps
           #:bing-news
           #:bing-shopping
           #:wordnet
           #:make-wikipedia-completion
           #:wikipedia
           #:make-yahoo-completion
           #:yahoo)
  (:documentation "A collection of search engines for Nyxt browser."))
