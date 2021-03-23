;;;; search-engines-mode.lisp

(in-package #:nx-search-engines)

(define-mode search-engines-mode ()
  "A mode to search hints in the dedicated search engine and image search engine."
  ((search-engine (nyxt::default-search-engine
                   (nyxt:search-engines (nyxt:current-buffer)))
                  :type (or nyxt:search-engine null)
                  :documentation "The search engine to use when calling `search-hint'")
   (image-search-engine (google-images)
                        :type (or nyxt:search-engine null)
                        :documentation "The search engine to use when calling `search-hint' on images.")))

(defmethod %search-hint ((hint nyxt/web-mode::hint))
  (nyxt:echo "Unsupported operation for hint: can't search ~S." (class-of hint)))

(defmethod %search-hint ((hint nyxt/web-mode::button-hint))
  (nyxt:buffer-load (nyxt::generate-search-query
                     (nyxt/web-mode::body hint)
                     (nyxt:search-url (search-engine (nyxt:find-submode (nyxt:current-buffer)
                                                                        'search-engines-mode))))))

(defmethod %search-hint ((hint nyxt/web-mode::link-hint))
  (nyxt:buffer-load (nyxt::generate-search-query
                     (nyxt/web-mode::url hint)
                     (nyxt:search-url (search-engine (nyxt:find-submode (nyxt:current-buffer)
                                                                        'search-engines-mode))))))

(nyxt:define-parenscript %input-area-get-text (&key nyxt-identifier)
  (defun qs (context selector)
    "Alias of document.querySelector"
    (ps:chain context (query-selector selector)))
  (ps:chain (qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" nyxt-identifier))) value))

(defmethod %search-hint ((hint nyxt/web-mode::input-hint))
  (nyxt:buffer-load (nyxt::generate-search-query
                     (%input-area-get-text :nyxt-identifier (nyxt/web-mode::identifier hint))
                     (nyxt:search-url (search-engine (nyxt:find-submode (nyxt:current-buffer)
                                                                        'search-engines-mode))))))
(defmethod %search-hint ((hint nyxt/web-mode::textarea-hint))
  (nyxt:buffer-load (nyxt::generate-search-query
                     (%input-area-get-text :nyxt-identifier (nyxt/web-mode::identifier hint))
                     (nyxt:search-url (search-engine (nyxt:find-submode (nyxt:current-buffer)
                                                                        'search-engines-mode))))))

(defmethod %search-hint ((hint nyxt/web-mode::image-hint))
  (nyxt:buffer-load (nyxt::generate-search-query
                     (nyxt/web-mode::url hint)
                     (nyxt:search-url (image-search-engine
                                       (nyxt:find-submode (nyxt:current-buffer)
                                                          'search-engines-mode))))))

(define-command search-hint (&key annotate-visible-only-p)
  "Search for the contents of the hint with default search engines.
In the case of links and input areas, a default search engine of Nyxt
is used (unless overridden by `search-engine').  In case of images,
`image-search-engine' is used."
  (nyxt/web-mode::query-hints "Search element" (lambda (results) (%search-hint (first results)))
                              :annotate-visible-only-p annotate-visible-only-p))

