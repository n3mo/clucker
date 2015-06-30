;;; clucker.scm --- Twitter API Acccess Egg For Chicken Scheme
;; Copyright 2014, Nicholas M. Van Horn
;; Author: Nicholas M. Van Horn <vanhorn.nm@gmail.com>
;; Keywords: twitter rest API scheme chicken
;; Version: 0.1
;;;;;

;;; TODO:
;;; + URL encode requests (or does rest-bind do this alread?) 
;;;   https://en.wikipedia.org/wiki/Percent-encoding

(module clucker *

  (import scheme chicken)

  (use extras irregex data-structures)
  (use openssl oauth-client uri-common rest-bind medea)

  ;; Twitter's streaming API endpoints allow tweets to be retrieved
  ;; indefinitely, but the user will want to be able to stop
  ;; collecting after receiving a set number of tweets and/or after a
  ;; set amount of time. These parameters control when the connection
  ;; to these endpoints are closed. They are set to very large numbers
  ;; by default, effectively leaving these connections open
  ;; indefinitely. The calling program should probably set these
  ;; parameters to something else before calling streaming endpoint
  ;; methods below
  (define max-tweets (make-parameter 999999999999999999))
  (define global-max-seconds (make-parameter 999999999999999999))

  ;; Each call to an API endpoint requires a reader---a procedure that
  ;; accepts a port and reads out the data (see
  ;; call-with-input-request from the http-client egg). Readers for
  ;; each api-endpoint are set individually as parameters. By default,
  ;; each reader is simply (read-line). Code using this egg can set
  ;; these readers on a case-by-case need to any other appropriate
  ;; procedure. Each reader parameter is named after the twitter api
  ;; endpoint url (converted to dashes), followed by -reader

  ;; (define statuses-sample-reader (make-parameter (lambda () (streaming-reader))))
  ;; (define statuses-filter-reader (make-parameter (lambda () (streaming-reader))))
  (define user-timeline-reader (make-parameter read-line))
  (define application-rate-limit-status-reader (make-parameter read-line))
  (define trends-available-reader (make-parameter read-line))
  
  ;; Lots of web services, including Twitter, don't accept ';' separated
  ;; query strings so use '&' for encoding by default but support both
  ;; '&' and ';' for decoding.
  (form-urlencoded-separator "&;")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; Oauth Procedures
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; Calls to Twitter's apis must be signed with oauth. The following
  ;; procedures provide the "service" and "token-credential" required
  ;; by with-oauth

  ;; This returns a structure for use as a "service" when calling
  ;; with-oauth 
  (define (twitter-service #!key consumer-key consumer-secret)
    (let ((twitter-provider
	   (make-oauth-service-provider
	    protocol-version: '1.0a
	    credential-request-url: "https://api.twitter.com/oauth/request_token"
	    owner-auth-url: "https://api.twitter.com/oauth/authorize"
	    token-request-url: "https://api.twitter.com/oauth/access_token"
	    signature-method: 'hmac-sha1)))
      (make-oauth-service
       service: twitter-provider
       client-credential: (make-oauth-credential consumer-key
						 consumer-secret))))
  
  ;; This returns a "token-credential" structure for use with
  ;; with-oauth 
  (define (twitter-token-credential #!key
				    access-token
				    access-token-secret)
    (make-oauth-credential access-token access-token-secret))
  
  ;; Taken together, the results of the above procedures can be used
  ;; with `with-auth`. Example:
  ;; (let ((twitter (twitter-service #:consumer-key "ssdkg24598sg"
  ;;   				     #:consumer-secret "98sf876sg8s76fg"))
  ;; 	(user-token (twitter-token-credential #:access-token "isydi7s6g"
  ;; 					      #:access-token-secret "ksdjhk25hj")))
  ;;   (with-oauth twitter user-token (lambda () ...)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; Helper Procedures
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; These procedures are useful for working with data retrieved from
  ;; Twitter's API and/or for using the clucker egg.

  ;; Taken from my "s" egg
  (define (s-replace old new s)
    (irregex-replace/all (irregex-quote old) s new))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; Custom Readers
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; These readers can be passed to methods created with rest-bind's
  ;; define-method to parse the retuned json into csv-friendly arrays.
  ;; Each custom reader needs its own header, which is a procedure that
  ;; returns a list of keys to be matched using alist-ref. These keys
  ;; will become the column names in tabular csv data. The order of the
  ;; header keys determines the order of the csv columns.

  ;; (define (search-twitter-reader result)
  ;;   (search-twitter->record (read-json result)))

  ;; (define (trends-place-header)
  ;;   '(name query url promoted_content woeid))

  ;; (define (trends-place-reader result)
  ;;   (trends-place->record (read-json result)))

  (define (generic-reader result)
    (let lp ((line (read-line result)))
      (unless (eof-object? line)
  	(display line)
	(newline)
  	(lp (read-line result)))))

  ;; (define (streaming-reader result)
  ;;   (let ((max-seconds (+ (current-seconds) (global-max-seconds))))
  ;;     (let lp ((line (read-line result))
  ;; 	       (num-tweets 0))
  ;; 	(if (and (<= num-tweets (max-tweets)) (< (current-seconds) max-seconds))
  ;; 	    (unless (eof-object? line)
  ;; 	      (cond ((not (string->number line))
  ;; 		     (begin
  ;; 		       (display line)
  ;; 		       (newline)
  ;; 		       (lp (read-line result) (+ num-tweets 1))))
  ;; 		    (else
  ;; 		     (lp (read-line result) num-tweets))))
  ;; 	    (close-input-port result)))))
  (define (streaming-reader result)
    (let ((max-seconds (+ (current-seconds) (global-max-seconds))))
      (let lp ((line (read-line result))
	       (num-tweets 0))
	(if (and (<= num-tweets (max-tweets)) (< (current-seconds) max-seconds))
	    (unless (eof-object? line)
	      (cond ((not (string->number line))
		     (begin
		       (display line)
		       (newline)
		       (lp (read-line result) (+ num-tweets 1))))
		    (else
		     (lp (read-line result) num-tweets))))
	    (close-input-port result)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; API Access Methods
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; You are unlikley to want to call these methods directly. They make
  ;; the calls to Twitter's API, but they do not manage such things as
  ;; pagniation of results and rate limits. Use the convenience
  ;; procedures below instead of these to simplify these steps.

  ;; Returns the available WOEID locations (for use with in other API
  ;; calls when restricting to given location(s). This API endpoint
  ;; requires no options.
  ;; (define-method (trends-available)
  ;;   "https://api.twitter.com/1.1/trends/available.json"
  ;;   #f generic-reader #f)
  (define-method (trends-available)
    "https://api.twitter.com/1.1/trends/available.json"
    #f (trends-available-reader) #f)

  ;; Fetch a user's timeline
  ;; (define-method (user-timeline #!key
  ;; 				user_id
  ;; 				screen_name
  ;; 				since_id
  ;; 				count
  ;; 				max_id
  ;; 				trim_user
  ;; 				exclude_replies
  ;; 				contributor_details
  ;; 				include_rts)
  ;;   "https://api.twitter.com/1.1/statuses/user_timeline.json"
  ;;   #f read-line #f)
  (define-method (user-timeline #!key
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
    #f (user-timeline-reader) #f)

  ;; Verify the user's credentials to ensure oauth signature is
  ;; working poperly
  (define-method (account-verify-credentials #!key
					     include_entities
					     skip_status
					     include_email)
    "https://api.twitter.com/1.1/account/verify_credentials.json"
    #f read-line #f)

  ;; ;;; Search the rest API
  (define-method (search-tweets #!key
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
    #f read-line #f)

  ;; Trends, top-10, for a given WOEID location
  (define-method (trends-place #!key id exclude)
    "https://api.twitter.com/1.1/trends/place.json"
    #f read-line #f)

  ;; (define-method (application-rate-limit-status #!key resources)
  ;;   "https://api.twitter.com/1.1/application/rate_limit_status.json"
  ;;   #f read-json #f)
  (define-method (application-rate-limit-status #!key resources)
    "https://api.twitter.com/1.1/application/rate_limit_status.json"
    #f (application-rate-limit-status-reader) #f)

  ;; Streaming API. This method runs forever, unless the global
  ;; parameters max-tweets or global-max-seconds are set to
  ;; something. If so, the statuses-filter-reader kills the connection
  ;; (rather crudely, due to limitations in chicken's http package
  (define-method (statuses-filter #!key delimited stall_warnings
					 follow track locations language)
    "https://stream.twitter.com/1.1/statuses/filter.json"
    #f streaming-reader #f)

  ;; Random access streaming endpoint. This returns a random sample of
  ;; all tweets (1% of them). No keywords required.
  ;; (define-method (statuses-sample #!key delimited stall_warnings)
  ;;   "https://stream.twitter.com/1.1/statuses/sample.json"
  ;;   #f statuses-filter-reader #f)
  (define-method (statuses-sample #!key delimited stall_warnings)
    "https://stream.twitter.com/1.1/statuses/sample.json"
    #f streaming-reader #f)

  ;; (define-method (debug-method #!key id exclude)
  ;;   "https://api.twitter.com/1.1/search/tweets.json"
  ;;   #f read-json #f)

  ;; Macro for building API methods
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

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; Clucker Procedures
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; These procedures are meant for direct use in building applications
  ;; using clucker. These follow the naming covention of removing the
  ;; suffix "-method" from their corresponding API Access Methods from
  ;; above. Keys offer optional API control (defaults follow Twitter's
  ;; documentation at https://dev.twitter.com/rest/public. Every
  ;; procedure requires as the first parameter "credentials" This is a
  ;; valid list as returned by make-clucker-credential

  ;; Search Twitter's rest API.
  ;; (define (search-twitter query #!key count)
  ;;   (search-twitter-method q: query count: count))

  ;; Work in progress below
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
  
  ) ;; end of module clucker

;;; clucker.scm ends here
