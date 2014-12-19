;;; clucker.scm --- Twitter API Acccess Egg For Chicken Scheme
;; Copyright 2014, Nicholas M. Van Horn
;; Author: Nicholas M. Van Horn <vanhorn.nm@gmail.com>
;; Keywords: twitter rest API scheme chicken
;; Version: 0.1
;;;;;

(import scheme chicken)
(use openssl oauth-client uri-common rest-bind medea)

; Lots of web services, including Twitter, don't accept ';' separated
; query strings so use '&' for encoding by default but support both
; '&' and ';' for decoding.
(form-urlencoded-separator "&;")

;;; The header, and subsequently the order of fields to be extracted
;;; from each status. The order of these entries matches that created
;;; by tweet->record
(define timeline-header
  '(text id screen_name user_id source lang created_at))

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
(define (tweet->record status)
  (list
   (alist-ref 'text status eqv? 'NA)
   (alist-ref 'id status eqv? 'NA)
   (alist-ref 'screen_name (alist-ref 'user status) eqv? 'NA)
   (alist-ref 'id (alist-ref 'user status) eqv? 'NA)
   (alist-ref 'source status eqv? 'NA)
   (alist-ref 'lang status eqv? 'NA)
   (alist-ref 'created_at status eqv? 'NA)))

;;; A wrapper that flattens all tweets in a list, and appends a header
;;; of fields to the resulting list. 
(define (twitter->list tweets header)
  (cons (map symbol->string header)
	(map (lambda (x) (tweet->record x)) tweets)))

;;; A simple csv writer. lst should be a list of lists. No checking is
;;; done to ensure that every list has the same number of elements
(define (write-csv lst file-path)
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
	   lst))))

;;; Clucker's body-reader for rest-bind methods
(define (twitter-reader result)
  (vector->list (alist-ref 'statuses (read-json result))))

;;; Makes a credential list for use in clucker procedures. oauth-service
;;; is a value as returned by make-oauth-service. oauth-credential is
;;; a value as returned by make-oauth-credential
;; (define (make-clucker-credential oauth-service oauth-credential)
;;   `((service . ,oauth-service) (credential . ,oauth-credential)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; API Access Methods
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;; You are unlikley to want to call these methods directly. They make
;;; the calls to Twitter's API, but provide no filtering of the
;;; returned data. They also do not manage such things as pagniation
;;; of results and rate limits. Use the convenience procedures below
;;; instead of these

;;; Let's fetch a user's timeline
(define-method (user-timeline-method #!key screen_name count)
  "https://api.twitter.com/1.1/statuses/user_timeline.json"
  #f twitter-reader #f)

;;; Search the rest API
;;; Let's fetch a user's timeline
(define-method (search-twitter-method #!key q count)
  "https://api.twitter.com/1.1/search/tweets.json"
  #f twitter-reader #f)

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
