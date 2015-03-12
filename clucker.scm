;;; clucker.scm --- Twitter API Acccess Egg For Chicken Scheme
;; Copyright 2014, Nicholas M. Van Horn
;; Author: Nicholas M. Van Horn <vanhorn.nm@gmail.com>
;; Keywords: twitter rest API scheme chicken
;; Version: 0.1
;;;;;

;;; TODO:
;;; + URL encode requests (or does rest-bind do this alread?) 
;;;   https://en.wikipedia.org/wiki/Percent-encoding

(import scheme chicken)
(use irregex openssl oauth-client uri-common rest-bind medea data-structures)

; Lots of web services, including Twitter, don't accept ';' separated
; query strings so use '&' for encoding by default but support both
; '&' and ';' for decoding.
(form-urlencoded-separator "&;")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Helper Procedures
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;; These procedures are useful for working with data retrieved from
;;; Twitter's API and/or for using the clucker egg.

;;; Let's convert each status into a single flattened list, keeping
;;; only the data fields that we care about. This is the preferred
;;; method... to use a supplied header to determine what is
;;; returned. The problem is that the tweet structure returned is a
;;; tree with nested fields. alist-ref cannot recursively search
;; (define (flatten-status status header)
;;   (map (lambda (x) (alist-ref x status)) header))

;;; A hard wired version of the above 
;; (define (tweet->record status)
;;   (list
;;    (alist-ref 'text status eqv? 'NA)
;;    (alist-ref 'id status eqv? 'NA)
;;    (alist-ref 'screen_name (alist-ref 'user status) eqv? 'NA)
;;    (alist-ref 'id (alist-ref 'user status) eqv? 'NA)
;;    (alist-ref 'source status eqv? 'NA)
;;    (alist-ref 'lang status eqv? 'NA)
;;    (alist-ref 'created_at status eqv? 'NA)))

;; ;;; A wrapper that flattens all tweets in a list, and appends a header
;; ;;; of fields to the resulting vector of vectors. 
;; (define (search-twitter->list tweets header)
;;   (cons (map symbol->string header) 
;; 	(map tweet->record  tweets)))

(define (search-twitter->record tweets)
  (let ((header (search-twitter-header))
	(tweets-list (vector->list (alist-ref 'statuses tweets))))
    (cons (map symbol->string header)
	  (map (lambda (status)
		 (list
		  (alist-ref 'text status eqv? 'NA)
		  (alist-ref 'id status eqv? 'NA)
		  (alist-ref 'screen_name (alist-ref 'user status) eqv? 'NA)
		  (alist-ref 'id (alist-ref 'user status) eqv? 'NA)
		  (alist-ref 'source status eqv? 'NA)
		  (alist-ref 'lang status eqv? 'NA)
		  (alist-ref 'created_at status eqv? 'NA)))
	       tweets-list))))

(define (trends-place->record result)
  (let* ((result-list (car (vector->list result)))
	 (header (trends-place-header))
	 (trends (vector->list (alist-ref 'trends result-list)))
	 (woeid (alist-ref 'woeid (car (vector->list (alist-ref 'locations result-list))))))
    (cons (map symbol->string header)
	  (map (lambda (status)
		 (list
		  (alist-ref 'name status)
		  (alist-ref 'query status)
		  (alist-ref 'url status)
		  (alist-ref 'promoted_content status)
		  woeid))
	       trends))))

;;; A simple csv writer. records should be a vector of vectors (e.g.,
;;; as returned by the ...->record family of procedures). No checking
;;; is done to ensure that every vector has the same number of
;;; elements!
(define (write-csv records file-path)
  (with-output-to-file file-path
    (lambda ()
      ;; Outer loop across rows
      (for-each (lambda (row)
		  ;; Inner loop across columns
		  (let column-loop ((fields row))
		    (if (null? fields)
			(newline)
			(let ((curr-field (car fields))
			      (final? (null? (cdr fields))))
			  (write curr-field)
			  (if (not final?) (display ","))
			  (column-loop (cdr fields))))))
		records))))

;;; Taken from my "s" egg
(define (s-replace old new s)
  (irregex-replace/all (irregex-quote old) s new))


;;; Makes a credential list for use in clucker procedures. oauth-service
;;; is a value as returned by make-oauth-service. oauth-credential is
;;; a value as returned by make-oauth-credential
;; (define (make-clucker-credential oauth-service oauth-credential)
;;   `((service . ,oauth-service) (credential . ,oauth-credential)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Custom Readers
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; These readers can be passed to methods created with rest-bind's
;; define-method to parse the retuned json into csv-friendly arrays.
;; Each custom reader needs its own header, which is a procedure that
;; returns a list of keys to be matched using alist-ref. These keys
;; will become the column names in tabular csv data. The order of the
;; header keys determines the order of the csv columns.

;;; Clucker's body-reader for rest-bind methods. This returns the raw
;;; results from twitter (after conversion from json). This is useful
;;; for inspecting the entire returned data from Twitter
;; (define (debug-reader result)
;;   (alist-ref 'statuses (read-json result)))

(define (search-twitter-header)
  '(text id screen_name user_id source lang created_at))

;; (define (search-twitter-reader result)
;;   (let ((data-header (search-twitter-header)))
;;     (search-twitter->list (vector->list (alist-ref 'statuses (read-json result))) data-header)))
(define (search-twitter-reader result)
  (search-twitter->record (read-json result)))

(define (trends-place-header)
  '(name query url promoted_content woeid))

(define (trends-place-reader result)
  (trends-place->record (read-json result)))

(define (user-timeline-reader)
  (user-timeline->record (read-json result)))

;;; Experimental streaming API reader (that writes)
(define (statuses-filter-reader result)
  (with-output-to-file "~/Desktop/clucker-dump.json"
    (lambda ()
      (print result))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; API Access Methods
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;; You are unlikley to want to call these methods directly. They make
;;; the calls to Twitter's API, but they do not manage such things as
;;; pagniation of results and rate limits. Use the convenience
;;; procedures below instead of these to simplify these steps.

;;; Fetch a user's timeline
(define-method (user-timeline-method #!key
				     user_id
				     screen_name
				     since_id
				     count
				     max_id
				     trim_user
				     exclude_replies
				     contributor_details
				     include_rts)
  "https://api.twitter.com/1.1/statuses/user_timeline.json"
  #f user-timeline-reader #f)

;; ;;; Search the rest API
(define-method (search-twitter-method #!key
				      q
				      geocode
				      lang
				      locale
				      result_type
				      count
				      until
				      since_id
				      max_id
				      include_entities
				      callback)
  "https://api.twitter.com/1.1/search/tweets.json"
  #f search-twitter-reader #f)

(define-method (trends-place-method #!key id exclude)
  "https://api.twitter.com/1.1/trends/place.json"
  #f trends-place-reader #f)

(define-method (application-rate-limit-status-method #!key resources)
  "https://api.twitter.com/1.1/application/rate_limit_status.json"
  #f read-json #f)

;;; Streaming API
(define-method (statuses-filter-method #!key delimited stall_warnings
				       filter_level language follow
				       track locations count with
				       replies stringify_friend_id)
  "https://stream.twitter.com/1.1/statuses/filter.json"
  #f statuses-filter-reader #f)


;; (define-method (debug-method #!key id exclude)
;;   "https://api.twitter.com/1.1/search/tweets.json"
;;   #f read-json #f)

;;; Macro for building API methods
;; (define-syntax twitter-method
;;   (syntax-rules ()
;;     ((twitter-method method-name
;; 		     url
;; 		     body-writer
;; 		     body-reader
;; 		     header-reader)
;;      (lambda ()`(define-method (,method-name #!rest)
;; 		  (string-append
;; 		   "https://api.twitter.com/1.1/"
;; 		   url
;; 		   ".json")
;; 		  body-writer
;; 		  body-reader
;; 		  header-reader)))))

;; (define-syntax make-twitter-method
;;   (ir-macro-transformer
;;    (lambda (form inject compare?)
;;      (let* ((args (cdr form))
;; 	    (method-name (first args))
;; 	    (url (second args)))
;;        `(define-method (,method-name)
;; 	  (string-append
;; 	   "https://api.twitter.com/1.1/"
;; 	   ,url
;; 	   ".json")
;; 	  #f
;; 	  twitter-reader
;; 	  #f)))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Clucker Procedures
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;; These procedures are meant for direct use in building applications
;;; using clucker. These follow the naming covention of removing the
;;; suffix "-method" from their corresponding API Access Methods from
;;; above. Keys offer optional API control (defaults follow Twitter's
;;; documentation at https://dev.twitter.com/rest/public. Every
;;; procedure requires as the first parameter "credentials" This is a
;;; valid list as returned by make-clucker-credential

;;; Search Twitter's rest API.
(define (search-twitter query #!key count)
  (search-twitter-method q: query count: count))

;;; Work in progress below
;; (define (search-twitter #!key
;; 			q
;; 			geocode
;; 			lang
;; 			locale
;; 			result_type
;; 			count
;; 			until
;; 			since_id
;; 			max_id
;; 			include_entities
;; 			callback)
;;   (let* ((tmp (application-rate-limit-status-method resources:
;; 						     "search"))
;; 	 (rate (cdr (car (cdr (car (alist-ref 'resources rate))))))
;; 	 (limit (alist-ref 'limit rate))
;; 	 (remain (alist-ref 'remaining rate)))
;;     (let search-loop ((limit remain))
;;       )))
  





