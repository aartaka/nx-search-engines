;;;; search-engines-mode.lisp

(in-package #:nx-search-engines)

(define-mode search-engines-mode ()
  "A mode to search hints in the dedicated search engine and image search engine."
  ((search-engine (default-search-engine)
                  :type (or search-engine null)
                  :documentation "The search engine to use when calling `search-hint'.")
   (image-search-engine (google-images)
                        :type (or search-engine null)
                        :documentation "The search engine to use when calling `search-hint' on images.")))

(defmethod %search-hint ((hint nyxt/web-mode::hint))
  (nyxt:echo "Unsupported operation for hint: can't search ~S." (class-of hint)))

(defmethod %search-hint ((hint nyxt/web-mode::button-hint))
  (nyxt:buffer-load (make-instance 'new-url-query
                                   :query (nyxt/web-mode::body hint)
                                   :engine (search-engine (current-mode 'search-engines)))))

(defmethod %search-hint ((hint nyxt/web-mode::link-hint))
  (nyxt:buffer-load (make-instance 'new-url-query
                                   :query (quri:render-uri (nyxt/web-mode::url hint))
                                   :engine (search-engine (current-mode 'search-engines)))))

(nyxt:define-parenscript %input-area-get-text (&key nyxt-identifier)
  (ps:chain (nyxt/ps:qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" nyxt-identifier))) value))

(defmethod %search-hint ((hint nyxt/web-mode::input-hint))
  (nyxt:buffer-load (make-instance 'new-url-query
                                   :query (%input-area-get-text
                                           :nyxt-identifier (nyxt/web-mode::identifier hint))
                                   :engine (search-engine (current-mode 'search-engines)))))
(defmethod %search-hint ((hint nyxt/web-mode::textarea-hint))
  (nyxt:buffer-load (make-instance 'new-url-query
                                   :query (%input-area-get-text
                                           :nyxt-identifier (nyxt/web-mode::identifier hint))
                                   :engine (search-engine (current-mode 'search-engines)))))

(defmethod %search-hint ((hint nyxt/web-mode::image-hint))
  (nyxt:buffer-load (make-instance 'new-url-query
                                   :query (quri:render-uri (nyxt/web-mode::url hint))
                                   :engine (image-search-engine (current-mode 'search-engines)))))

(define-command search-hint (&key annotate-visible-only-p)
  "Search for the contents of the hint with default search engines.
In the case of links and input areas, a default search engine of Nyxt
is used (unless overridden by `search-engine').  In case of images,
`image-search-engine' is used."
  (nyxt/web-mode::query-hints "Search element" (lambda (results) (%search-hint (first results)))
                              :annotate-visible-only-p annotate-visible-only-p))

