;;;; search-engines.lisp

(in-package #:nx-search-engines)

(defmacro define-search-engine (name (&key shortcut fallback-url base-search-url completion-function
                                        documentation)
                                &body keywords)
  "Defines a new `nyxt:search-engine' called NAME and having FALLBACK-URL.
`nyxt:search-url' of the new engine is built from BASE-SEARCH-URL and KEYWORDS.

BASE-SEARCH-URL is a control string with one placeholder (e.g.,
\"example.com/?q=~a\") each keyword in KEYWORDS is a list of a
form (KEYWORD URI-PARAMETER VALUES), where VALUES is either
- an association list of possible values and their URI representations, or
- a function to process the value provided by the user.

Example:"
  (flet ((supplied-p (symbol)
           (intern (format nil "~s-SUPPLIED-P" symbol)
                   (symbol-package symbol)))
         (make-cond (arg-name values)
           `(cond
              ,@(loop :for value :in values
                      :collect
                      `((equal ,arg-name ,(first value))
                        ,(second value))
                      :into clauses
                      :finally (return (append clauses (list `(t ,arg-name))))))))
    `(progn
       (serapeum:export-always (quote ,name))
       (defun ,name (&key
                       (fallback-url ,fallback-url)
                       (shortcut ,shortcut)
                       (completion-function ,completion-function)
                       ,@(mapcar #'(lambda (k)
                                     (list (first k)                 ; name
                                           (if (eq (first (third k)) :function)
                                               nil
                                               (first (first (third k)))) ; default value
                                           (supplied-p (first k))))  ; supplied-p
                                 keywords))
         ,documentation
         (make-instance
          'search-engine
          :shortcut shortcut
          :fallback-url fallback-url
          :completion-function completion-function
          :search-url (format nil "~a~{~a~}"
                              ,base-search-url
                              (delete
                               nil
                               (list
                                ,@(loop :for (arg-name uri-parameter values)
                                          :in keywords
                                        :collect
                                        `(when ,(supplied-p arg-name)
                                           (format nil "&~a=~a"
                                                   ,uri-parameter
                                                   ,(if (eq (first values) :function)
                                                        `(funcall ,(second values) ,arg-name)
                                                        (make-cond arg-name values)))))))))))))

(defmacro define-derived-search-engine (name (parent-engine &rest arguments))
  ;; TODO: Use `mopu:function-arglist' to reproduce original arglist?
  `(progn
     (serapeum:export-always (quote ,name))
     (defun ,name (&rest args)
      (apply (function ,parent-engine) ,@arguments args))))

(defun make-duckduckgo-completion (&key request-args)
  "Helper that generates DuckDuckGo search completion functions. The only
thing that's left to pass to it is REQUEST-ARGS to slightly modify the
request."
  (make-search-completion-function
   :base-url "https://duckduckgo.com/ac/?q=~a"
   :processing-function
   #'(lambda (results)
       (mapcar #'cdar
               (json:decode-json-from-string results)))
   :request-args request-args))

(define-search-engine duckduckgo
    (:shortcut "duckduckgo"
     :fallback-url "https://duckduckgo.com/"
     :base-search-url "https://duckduckgo.com/?q=~a"
     :completion-function (make-duckduckgo-completion)
     :documentation "DuckDuckGo `nyxt:search-engine' with the configuration as capable as the built-in settings pane.
See DuckDuckGo settings for the names of the necessary setting and use
the matching kebab-case keywords for this helper.")
  ;; DuckDuckGo uses two (four?) object-related keywords.
  (object "ia" ((:all "web")
                (:images "images")
                (:videos "videos")
                (:news "news")
                (:meanings "meanings")))
  (object2 "iax" ((:all "web")
                  (:images "images")
                  (:videos "videos")
                  (:news "news")
                  (:meanings "meanings")))
  ;;; Theming
  (theme "kae" ((:default  "")
                (:basic    "b")
                (:contrast "c")
                (:dark     "d")
                (:gray     "g")
                (:terminal "t")))
  ;;; Privacy
  (get-requests "kg" ((t "")
                      (nil "p"))) ;; Use POST requests.
  (video-playback "k5" ((:prompt-me "")
                        (:always-ddg "1")
                        (:always-third-party "2")))
  ;; See https://help.duckduckgo.com/results/rduckduckgocom/
  (redirect "kd" ((t "")
                  (nil "-1")))
  ;;; General
  ;; abbr. means the abbreviation I came up with based on what DDG
  ;; provides. It's mostly just removing the -en suffix from the
  ;; region name, like :india-en -> :india
  (region "kl" ((:all "")
                (:argentina   "ar-es")
                (:australia   "au-en")
                (:austria     "at-de")
                (:belgium-fr  "be-fr")
                (:belgium-nl  "be-nl")
                (:brazil      "br-pt")
                (:bulgaria    "bg-bg")
                (:canada-en   "ca-en")
                (:canada-fr   "ca-fr")
                (:catalonia   "ct-ca")
                (:chile       "cl-es")
                (:china       "cn-zh")
                (:colombia    "co-es")
                (:croatia     "hr-hr")
                (:czech-republic "cz-cs")
                (:denmark     "dk-da")
                (:estonia     "ee-et")
                (:finland     "fi-fi")
                (:france      "fr-fr")
                (:germany     "de-de")
                (:greece      "gr-el")
                (:hong-kong   "hk-tzh")
                (:hungary     "hu-hu")
                (:india-en    "in-en")
                (:india       "in-en")   ; abbr.
                (:indonesia-en "id-en")
                (:indonesia   "id-en")   ; abbr.
                (:ireland     "ie-en")
                (:israel-en   "il-en")
                (:israel      "il-en")   ; abbr.
                (:italy       "it-it")
                (:japan       "jp-jp")
                (:korea       "kr-kr")
                (:latvia      "lv-lv")
                (:lithuania   "lt-lt")
                (:malaysia-en "my-en")
                (:malaysia    "my-en")   ; abbr.
                (:mexico      "mx-es")
                (:netherlands "nl-nl")
                (:new-zealand "nz-en")
                (:norway      "no-no")
                (:pakistan-en "pk-en")
                (:peru        "pe-es")
                (:philippines-en "ph-en")
                (:philippines "ph-en")   ; abbr.
                (:poland      "pl-pl")
                (:portugal    "pt-pt")
                (:romania     "ro-ro")
                (:russia      "ru-ru")
                (:russian-federation "ru-ru") ; abbr.
                (:saudi-arabia "xa-ar")
                (:singapore   "sg-en")
                (:slovakia    "sk-sk")
                (:slovenia    "sl-sl")
                (:south-africa "za-en")
                (:spain-ca    "es-ca")
                (:spain-es    "es-es")
                (:spain       "es-es")   ; abbr.
                (:sweden      "se-sv")
                (:switzerland-de "ch-de")
                (:switzerland-fr "ch-fr")
                (:taiwan "tw-tzh")
                (:thailand-en "th-en")
                (:thailand    "th-en")   ; abbr.
                (:turkey      "tr-tr")
                (:us-english  "us-en")
                (:us-en       "us-en")   ; abbr.
                (:us          "us-en")   ; abbr.
                (:us-spanish  "us-es")
                (:us-es       "us-es")   ; abbr.
                (:ukraine     "ua-uk")
                (:united-kingdom "uk-en")
                (:uk "uk-en")            ; abbr.
                (:vietnam-en  "vn-en")
                (:vietnam     "vn-en"))) ; abbr.
  ;; TODO: Write it.
  (language "kad" ((:default "")))
  (safe-search "kp" ((:moderate "")
                     (:strict   "1")
                     (:off      "-2")))
  (instant-answers "kz" ((t "")
                         (nil "-1")))
  (infinite-scroll-for-media "kav" ((t "")
                                    (nil "-1")))
  (infinite-scroll "kav" ((nil "")
                          (t "1")))
  (autocomplete-suggestions "kac" ((t "")
                                   (nil "-1")))
  (open-in-new-tab "kn" ((nil "")
                         (t   "1")))
  (advertisements "k1"  ((t "")
                         (nil "-1")))
  (keyboard-shortcuts "kk" ((t "")
                            (nil "-1")))
  (units-of-measure "kaj" ((:no-preference "")
                           (:metric "m")
                           (:us-based "u")))
  (map-rendering "kay" ((:not-set "")
                        (:best-available "b")
                        (:image-tiles "i")))
  (page-break-numbers "kv" ((:on "")
                            (:off "-1")
                            (:lines "l")))
  (install-duckduckgo "kak" ((t "")
                             (nil "-1")))
  (install-reminders "kax" ((t "")
                            (nil "-1")))
  (privacy-newsletter "kaq" ((t "")
                             (nil "-1")))
  (newsletter-reminders "kap" ((t "")
                               (nil "-1")))
  (homepage-privacy-tips "kao" ((t "")
                                (nil "-1")))
  (help-improve-duckduckgo "kau" ((t "")
                                  (nil "-1")))
  ;;; Appearance
  (font "kt" (("Proxima Nova" "")
              (:proxima-nova  "")
              ("Arial" "a")
              (:arial "a")
              ("Century Gothic" "c")
              (:century-gothic "c")
              ("Georgia" "g")
              (:georgia "g")
              ("Helvetica" "h")
              (:helvetica "h")
              ("Helvetica Neue" "u")
              (:helvetica-neue "u")
              ("Sans Serif" "n")
              (:sans-serif "n")
              ("Segoe UI" "e")
              (:segoe-ui "e")
              ("Serif" "s")
              (:serif "s")
              ("Times" "t")
              (:times "t")
              ("Tahoma" "o")
              (:tahoma "o")
              ("Trebuchet MS" "b")
              (:trebuchet-ms "b")
              ("Verdana" "v")
              (:verdana "v")))
  (font-size "ks" ((:large "")
                   (:small "s")
                   (:medium "m")
                   (:larger "l")
                   (:largest "t")))
  (page-width "kw" ((:normal "")
                    (:wide "w")
                    (:super-wide "s")))
  (center-alignment "km" ((nil "")
                          (t "m")))
  (background-color "k7" ((:default "ffffff")))
  (header-behavior "ko" ((:on-dynamic "")
                         (:on-fixed "1")
                         (:off "-1")
                         (:on-scrolling "s")))
  (header-color "kj" ((:default "ffffff")))
  (result-title-font "ka" (("Proxima Nova" "")
                           (:proxima-nova  "")
                           ("Arial" "a")
                           (:arial "a")
                           ("Century Gothic" "c")
                           (:century-gothic "c")
                           ("Georgia" "g")
                           (:georgia "g")
                           ("Helvetica" "h")
                           (:helvetica "h")
                           ("Helvetica Neue" "u")
                           (:helvetica-neue "u")
                           ("Sans Serif" "n")
                           (:sans-serif "n")
                           ("Segoe UI" "e")
                           (:segoe-ui "e")
                           ("Serif" "s")
                           (:serif "s")
                           ("Times" "t")
                           (:times "t")
                           ("Tahoma" "o")
                           (:tahoma "o")
                           ("Trebuchet MS" "b")
                           (:trebuchet-ms "b")
                           ("Verdana" "v")
                           (:verdana "v")))
  (result-title-color "k9" ((:default "084999")))
  (result-visited-title-color "kaa" ((:default "6c00a2")))
  (result-title-underline "ku" ((nil "")
                                (t "1")))
  (result-description-color "k8" ((:default "494949")))
  (result-url-color "kx" ((:default "3f6e35")))
  (result-module-color "k21" ((:default "ffffff")))
  (result-full-urls "kaf" ((t "")
                           (nil "-1")))
  (result-urls-above-snippet "kaf" ((t "")
                                    (nil "-1")))
  (result-visible-checkmark "k18" ((nil "")
                                   (t "1")))
  (site-icons "kf" ((t "")
                    (nil "-1"))))

(define-derived-search-engine duckduckgo-images
    (duckduckgo :object :images :object2 :images))

(defun make-google-completion (&key request-args)
  "Helper that generates Google search completion functions. The only
thing that's left to pass to it is REQUEST-ARGS to slightly modify the
request."
  (make-search-completion-function
   :base-url "https://www.google.com/complete/search?q=~a&client=gws-wiz"
   :processing-function
   #'(lambda (results)
       (mapcar (alexandria:compose (alexandria:curry #'str:replace-using '("<b>" "" "</b>" ""))
                                   #'first)
               (first (json:decode-json-from-string
                       (str:replace-first "window.google.ac.h(" "" results)))))
   :request-args request-args))

(define-search-engine google
    (:shortcut "google"
     :fallback-url "google.com"
     :base-search-url "google.com/search?q=~a"
     :completion-function (make-google-completion)
     :documentation "Google `nyxt:search-engine'.
Does not support advanced results sorting as of now.
Arguments:
SAFE-SEARCH -- Whether results will be filtered. Boolean. t to enable,
nil to disable.
OBJECT -- One of :all :image, :video, :news, :shopping, :books,
:finance.")
  (safe-search "safe" ((t   "strict")
                       (nil "images")))
  (object "tbm" ((:all      "")
                 (:image    "isch")
                 (:video    "vid")
                 (:news     "nws")
                 (:shopping "shop")
                 (:books    "bks")
                 (:finance  "fin"))))

(define-derived-search-engine google-images
    (google :object :image))

(serapeum:export-always 'bing-date)
(declaim (ftype (function (local-time:timestamp local-time:timestamp) string) bing-date))
(defun bing-date (start-date end-date)
  "Helper function generating Bing-acceptable dates in the form \"ez5_START-DATE_END-DATE\".
Use it for the value of :date argument to `bing'"
  (format nil "\"ez5_~d_~d\""
          (local-time:day-of
           (local-time:timestamp-
            start-date (local-time:day-of (local-time:unix-to-timestamp 0)) :day))
          (local-time:day-of
           (local-time:timestamp-
            end-date (local-time:day-of (local-time:unix-to-timestamp 0)) :day))))

(define-search-engine bing
    (:shortcut "bing"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing.
MY-LANGUAGE-ONLY and MY-COUNTRY-ONLY are pre-defined by Bing based on
your location. Both are booleans.
DATE is either :all, :past-24-hours, :past-week, :past-month,
:past-year or an arbitrary bing-acceptable time string that you can
generate with `bing-date'.")
  (my-language-only "lf" ((nil "")
                          (t "1")))
  (my-countly-only "rf" ((nil "")
                         (t "1")))
  (date "filters" ((:all "")
                   (:past-24-hours "\"ez1\"")
                   (:past-week "\"ez2\"")
                   (:past-month "\"ez3\"")
                   (:past-year (bing-date (local-time:timestamp- (local-time:now) 1 :year)
                                          (local-time:now))))))

(define-search-engine bing-images
    (:shortcut "bing-images"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/images/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing Images."))

(define-search-engine bing-videos
    (:shortcut "bing-videos"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/videos/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing Videos."))

(define-search-engine bing-maps
    (:shortcut "bing-maps"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/maps/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing Maps."))

(define-search-engine bing-news
    (:shortcut "bing-news"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/news/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing News.

INTERVAL is the time since news publishing. It can be one of :all,
:past-5-minutes, :past-15-minutes, :past-30-minutes, :past-hour,
:past-4-hours, :past-6-hours, :past-24-hours, :past-day, :past-7-days,
:past-week, :past-30-days, :past-month. Yes, some are duplicated.")
  (interval "qft" ((:all "")
                   (:past-5-minutes "interval=\"1\"")
                   (:past-15-minutes "interval=\"2\"")
                   (:past-30-minutes "interval=\"3\"")
                   (:past-hour "interval=\"4\"")
                   (:past-4-hours "interval=\"5\"")
                   (:past-6-hours "interval=\"6\"")
                   (:past-24-hours "interval=\"7\"")
                   (:past-day "interval=\"7\"")
                   (:past-7-days "interval=\"8\"")
                   (:past-week "interval=\"8\"")
                   (:past-30-days "interval=\"9\"")
                   (:past-month "interval=\"9\""))))

(define-search-engine bing-shopping
    (:shortcut "bing-shopping"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/shopping/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing Shopping."))

(define-search-engine wordnet (:shortcut "wordnet"
                               :fallback-url (quri:uri "http://wordnetweb.princeton.edu/perl/webwn")
                               :base-search-url "http://wordnetweb.princeton.edu/perl/webwn?s=~a"
                               :documentation "`nyxt:search-engine' for WordNet.

To use it, disable force-https-mode for wordnetweb.princeton.edu or
add auto-mode rule that will manage that for you:

((match-host \"wordnetweb.princeton.edu\") :excluded (nyxt/force-https-mode:force-https-mode))

Arguments mean:
SHORTCUT -- the shortcut you need to input to use this search engine. Set to \"wordnet\" by default.
FALLBACK-URL -- what URL to follow if there was a search error.
SHOW-EXAMPLES -- Show example sentences. T by default.
SHOW-GLOSSES -- Show definitions. T by default.
SHOW-WORD-FREQUENCIES -- Show word frequency counts. NIL by default.
SHOW-DB-LOCATIONS -- Show WordNet database locations for this word. NIL by default.
SHOW-LEXICAL-FILE-INFO -- Show lexical file word belongs to. NIL by default.
SHOW-LEXICAL-FILE-NUMBERS -- Show number of the word in the lexical file. NIL by default.
SHOW-SENSE-KEYS -- Show symbols for senses of the word. NIL by default.
SHOW-SENSE-NUMBERS -- Show sense numbers. NIL by default.

A sensible non-default example:
\(wordnet :shortcut \"wn\"
         :show-examples nil
         :show-word-frequencies t
         :show-sense-numbers t)

This search engine, invokable with \"wn\", will show:
- NO example sentences,
- glosses,
- frequency counts,
- sense-numbers.")
  (show-examples             "o0" ((t "1")  (nil "")))
  (show-glosses              "o1" ((t "1")  (nil "")))
  (show-word-frequencies     "o2" ((nil "") (t "1")))
  (show-db-locations         "o3" ((nil "") (t "1")))
  (show-lexical-file-info    "o4" ((nil "") (t "1")))
  (show-lexical-file-numbers "o5" ((nil "") (t "1")))
  (show-sense-keys           "o6" ((nil "") (t "1")))
  (show-sense-numbers        "o7" ((nil "") (t "1"))))

;; TODO:
;; - Google Images
;; - YouTube
;; - Yahoo
;; - Amazon
;; - Facebook
;; - Gmaps
;; - Twitter
;; - Pinterest
;; - Ask
;; - Baidu
;; - WolframAlpha
;; - Boardreader
;; - StartPage
;; - Ecosia
;; - Qwant
;; - Search Encrypt
;; - SearX
;; - Yandex
;; - Yandex.Images
;; - Gibiru
;; - Disconnect
;; - Yippy
;; - Swisscows
;; - Lukol
;; - Metager
;; - Gigablast
;; - Oskobo
;; - Infinity Search
;; - Mail.ru
;; - Rambler.ru
