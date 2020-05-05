;;; clucker.scm --- Twitter API Acccess Egg For Chicken Scheme

;; Author: Nicholas M. Van Horn <vanhorn.nm@gmail.com>
;; Keywords: twitter api scheme chicken
;; Version: 0.11
;; Repo: https://github.com/n3mo/clucker/

;; Copyright (c) 2015-2020, Nicholas M. Van Horn
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the authors may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Commentary:
;; Provides bindings to Twitter's public API access endpoints. All
;; public streaming endpoints are supported, as well as all GET REST
;; api endpoints

;; TODO: Add support for POST REST api endpoints

(module clucker *

  (cond-expand
    (chicken-4
     (import scheme chicken)
     (use extras irregex data-structures)
     (use oauth-client uri-common rest-bind))
    (else
     (import scheme)
     (import (chicken irregex) (chicken base) (chicken io) (chicken time)
	     (chicken string))
     (import oauth-client uri-common rest-bind)))

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

  (define statuses-mentions-timeline-reader (make-parameter read-line))
  (define statuses-user-timeline-reader (make-parameter read-line))
  (define statuses-home-timeline-reader (make-parameter read-line))
  (define statuses-retweets-of-me-reader (make-parameter read-line))
  (define statuses-retweets-:id-reader (make-parameter read-line))
  (define statuses-show-:id-reader (make-parameter read-line))
  (define statuses-retweeters-ids-reader (make-parameter read-line))
  (define statuses-lookup-reader (make-parameter read-line))
  (define direct-messages-sent-reader (make-parameter read-line))
  (define direct-messages-show-reader (make-parameter read-line))
  (define search-tweets-reader (make-parameter read-line))
  (define direct-messages-reader (make-parameter read-line))
  (define friendships-no-retweets-ids-reader (make-parameter read-line))
  (define friends-ids-reader (make-parameter read-line))
  (define followers-ids-reader (make-parameter read-line))
  (define friendships-incoming-reader (make-parameter read-line))
  (define friendships-outgoing-reader (make-parameter read-line))
  (define friendships-show-reader (make-parameter read-line))
  (define friends-list-reader (make-parameter read-line))
  (define followers-list-reader (make-parameter read-line))
  (define friendships-lookup-reader (make-parameter read-line))
  (define account-settings-reader (make-parameter read-line))
  (define account-verify-credentials-reader (make-parameter read-line))
  (define blocks-list-reader (make-parameter read-line))
  (define blocks-ids-reader (make-parameter read-line))
  (define users-lookup-reader (make-parameter read-line))
  (define users-show-reader (make-parameter read-line))
  (define users-search-reader (make-parameter read-line))
  (define users-profile-banner-reader (make-parameter read-line))
  (define mutes-users-ids-reader (make-parameter read-line))
  (define mutes-users-list-reader (make-parameter read-line))
  (define users-suggestions-:slug-reader (make-parameter read-line))
  (define users-suggestions-reader (make-parameter read-line))
  (define users-suggestions-:slug-members-reader (make-parameter read-line))
  (define favorites-list-reader (make-parameter read-line))
  (define lists-list-reader (make-parameter read-line))
  (define lists-statuses-reader (make-parameter read-line))
  (define lists-memberships-reader (make-parameter read-line))
  (define lists-subscribers-reader (make-parameter read-line))
  (define lists-subscribers-show-reader (make-parameter read-line))
  (define lists-members-show-reader (make-parameter read-line))
  (define lists-members-reader (make-parameter read-line))
  (define lists-show-reader (make-parameter read-line))
  (define lists-subscriptions-reader (make-parameter read-line))
  (define lists-ownerships-reader (make-parameter read-line))
  (define saved-searches-list-reader (make-parameter read-line))
  (define saved-searches-show-:id-reader (make-parameter read-line))
  (define geo-id-:place-id-reader (make-parameter read-line))
  (define geo-reverse-geocode-reader (make-parameter read-line))
  (define geo-search-reader (make-parameter read-line))
  (define trends-place-reader (make-parameter read-line))
  (define trends-available-reader (make-parameter read-line))
  (define application-rate-limit-status-reader (make-parameter read-line))
  (define help-configuration-reader (make-parameter read-line))
  (define help-languages-reader (make-parameter read-line))
  (define help-privacy-reader (make-parameter read-line))
  (define help-tos-reader (make-parameter read-line))
  (define trends-closest-reader (make-parameter read-line))

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
  ;; Custom Readers
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  (define (generic-reader result)
    (let lp ((line (read-line result)))
      (unless (eof-object? line)
  	(display line)
	(newline)
  	(lp (read-line result)))))

  ;; This reader should be used with all streaming API endpoints. It
  ;; closes streaming connections after global-max-seconds have passed
  ;; and/or when max-tweets have been collected. The connection is
  ;; closed quite crudely, and requires the calling procedure to
  ;; handle the inevitable exception that will be raised. After much
  ;; debate on IRC (#chicken), group consensus was that this was the
  ;; *only* option available
  (define (streaming-reader result)
    (let ((max-seconds (+ (current-seconds) (global-max-seconds))))
      (let lp ((line (read-line result))
	       (num-tweets 0))
	(if (and (< num-tweets (max-tweets)) (< (current-seconds) max-seconds))
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
  ;; Streaming API Access Methods
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; Searchable streaming API. This method runs forever, unless the
  ;; global parameters max-tweets or global-max-seconds are set to
  ;; something. If so, the statuses-filter-reader kills the connection
  ;; (rather crudely, due to limitations in chicken's http package)
  (define-method (statuses-filter #!key delimited stall_warnings
				  follow track locations language)
    "https://stream.twitter.com/1.1/statuses/filter.json"
    #f streaming-reader #f)

  ;; Random access streaming endpoint. This returns a random sample of
  ;; all tweets (1% of them). No keywords required. This method runs
  ;; forever, unless the global parameters max-tweets or
  ;; global-max-seconds are set to something. If so, the
  ;; statuses-filter-reader kills the connection (rather crudely, due
  ;; to limitations in chicken's http package)
  (define-method (statuses-sample #!key delimited stall_warnings)
    "https://stream.twitter.com/1.1/statuses/sample.json"
    #f streaming-reader #f)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; REST API Access Methods
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  ;; For a full explanation of each endpoint's behavior, see
  ;; https://dev.twitter.com/rest/public 

  ;; -------------------------------------------------------------------------
  (define-method (statuses-mentions-timeline #!key count since_id
					    max_id trim_user
					    contributor_details
					    include_entities)
    "https://api.twitter.com/1.1/statuses/mentions_timeline.json"
    #f (statuses-mentions-timeline-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (statuses-user-timeline #!key user_id screen_name since_id
				count max_id trim_user exclude_replies
				contributor_details include_rts) 
    "https://api.twitter.com/1.1/statuses/user_timeline.json"
    #f (statuses-user-timeline-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (statuses-home-timeline #!key count since_id max_id
					 trim_user
					 exclude_replies
					 contributor_details
					 include_entities)  
    "https://api.twitter.com/1.1/statuses/home_timeline.json"
    #f (statuses-home-timeline-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (statuses-retweets-of-me #!key count since_id max_id
					  trim_user include_entities
					  include_user_entities) 
    "https://api.twitter.com/1.1/statuses/retweets_of_me.json"
    #f (statuses-retweets-of-me-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (statuses-retweets-:id #!key id count trim_user)
    "https://api.twitter.com/1.1/statuses/retweets/:id.json"
    #f (statuses-retweets-:id-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (statuses-show-:id #!key id trim_user
				    include_my_retweet
				    include_entities) 
    "https://api.twitter.com/1.1/statuses/show.json"
    #f (statuses-show-:id-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (statuses-retweeters-ids #!key id cursor stringify_ids)
    "https://api.twitter.com/1.1/statuses/retweeters/ids.json"
    #f (statuses-retweeters-ids-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (statuses-lookup #!key id include_entities trim_user
				  map tweet_mode)
    "https://api.twitter.com/1.1/statuses/lookup.json"
    #f (statuses-lookup-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (direct-messages-sent #!key since_id max_id count
				       page include_entities)
    "https://api.twitter.com/1.1/direct_messages/sent.json"
    #f (direct-messages-sent-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (direct-messages-show #!key id)
    "https://api.twitter.com/1.1/direct_messages/show.json"
    #f (direct-messages-show-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (search-tweets #!key q geocode lang locale
				result_type count until since_id
				max_id include_entities callback) 
    "https://api.twitter.com/1.1/search/tweets.json"
    #f (search-tweets-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (direct-messages #!key since_id max_id count
				  include_entities skip_status) 
    "https://api.twitter.com/1.1/direct_messages.json"
    #f (direct-messages-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (friendships-no-retweets-ids #!key stringify_ids) 
    "https://api.twitter.com/1.1/friendships/no_retweets/ids.json"
    #f (friendships-no-retweets-ids-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (friends-ids #!key user_id screen_name cursor
			      stringify_ids count) 
    "https://api.twitter.com/1.1/friends/ids.json"
    #f (friends-ids-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (followers-ids #!key user_id screen_name cursor
			      stringify_ids count) 
    "https://api.twitter.com/1.1/followers/ids.json"
    #f (followers-ids-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (friendships-incoming #!key cursor stringify_ids) 
    "https://api.twitter.com/1.1/friendships/incoming.json"
    #f (friendships-incoming-reader) #f)
  
  ;; -------------------------------------------------------------------------
  (define-method (friendships-outgoing #!key cursor stringify_ids) 
    "https://api.twitter.com/1.1/friendships/outgoing.format"
    #f (friendships-outgoing-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (friendships-show #!key source_id source_screen_name
				   target_id target_screen_name) 
    "https://api.twitter.com/1.1/friendships/show.json"
    #f (friendships-show-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (friends-list #!key user_id screen_name cursor count
			       skip_status include_user_entities) 
    "https://api.twitter.com/1.1/friends/list.json"
    #f (friends-list-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (followers-list #!key user_id screen_name cursor count
			       skip_status include_user_entities) 
    "https://api.twitter.com/1.1/followers/list.json"
    #f (followers-list-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (friendships-lookup #!key screen_name user_id) 
    "https://api.twitter.com/1.1/friendships/lookup.json"
    #f (friendships-lookup-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (account-settings) 
    "https://api.twitter.com/1.1/account/settings.json"
    #f (account-settings-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (account-verify-credentials #!key include_entities
					     skip_status
					     include_email) 
    "https://api.twitter.com/1.1/account/verify_credentials.json"
    #f (account-verify-credentials-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (blocks-list #!key include_entities skip_status cursor) 
    "https://api.twitter.com/1.1/blocks/list.json"
    #f (blocks-list-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (blocks-ids #!key stringify_ids cursor) 
    "https://api.twitter.com/1.1/blocks/ids.json"
    #f (blocks-ids-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (users-lookup #!key screen_name user_id
			       include_entities)  
    "https://api.twitter.com/1.1/users/lookup.json"
    #f (users-lookup-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (users-show #!key user_id screen_name include_entities)  
    "https://api.twitter.com/1.1/users/show.json"
    #f (users-show-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (users-search #!key q page count include_entities)  
    "https://api.twitter.com/1.1/users/search.json"
    #f (users-search-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (users-profile-banner #!key user_id screen_name)  
    "https://api.twitter.com/1.1/users/profile_banner.json"
    #f (users-profile-banner-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (mutes-users-ids #!key cursor)  
    "https://api.twitter.com/1.1/mutes/users/ids.json"
    #f (mutes-users-ids-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (mutes-users-list #!key cursor include_entities
				   skip_status)   
    "https://api.twitter.com/1.1/mutes/users/list.json"
    #f (mutes-users-list-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (users-suggestions-:slug #!key slug lang)   
    "https://api.twitter.com/1.1/users/suggestions/:slug.json"
    #f (users-suggestions-:slug-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (users-suggestions-:slug-members #!key slug)   
    "https://api.twitter.com/1.1/users/suggestions/:slug/members.json"
    #f (users-suggestions-:slug-members-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (favorites-list #!key user_id screen_name count
				 since_id max_id include_entities)   
    "https://api.twitter.com/1.1/favorites/list.json"
    #f (favorites-list-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (lists-list #!key user_id screen_name reverse)   
    "https://api.twitter.com/1.1/lists/list.json"
    #f (lists-list-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (lists-statuses #!key list_id slug owner_screen_name
				 owner_id since_id max_id count
				 include_entities include_rts)   
    "https://api.twitter.com/1.1/lists/statuses.json"
    #f (lists-statuses-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (lists-memberships #!key user_id screen_name count cursor)   
    "https://api.twitter.com/1.1/lists/memberships.json"
    #f (lists-memberships-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (lists-subscribers #!key list_id slug
				    owner_screen_name owner_id count
				    cursor include_entities skip_status)   
    "https://api.twitter.com/1.1/lists/subscribers.json"
    #f (lists-subscribers-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (lists-subscribers-show #!key owner_screen_name
					 owner_id list_id slug user_id
					 screen_name include_entities
					 skip_status)    
    "https://api.twitter.com/1.1/lists/subscribers/show.json "
    #f (lists-subscribers-show-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (lists-members-show #!key list_id slug user_id
				     screen_name owner_screen_name
				     owner_id include_entities
				     skip_status)     
    "https://api.twitter.com/1.1/lists/members/show.json "
    #f (lists-members-show-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (lists-members #!key list_id slug owner_screen_name
				owner_id count cursor include_entities
				skip_status) 
    "https://api.twitter.com/1.1/lists/members.json"
    #f (lists-members-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (lists-show #!key list_id slug owner_screen_name
			     owner_id) 
    "https://api.twitter.com/1.1/lists/show.json"
    #f (lists-show-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (lists-subscriptions #!key user_id screen_name count cursor) 
    "https://api.twitter.com/1.1/lists/subscriptions.json"
    #f (lists-subscriptions-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (lists-ownerships #!key user_id screen_name count cursor) 
    "https://api.twitter.com/1.1/lists/ownerships.json"
    #f (lists-ownerships-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (saved-searches-list) 
    "https://api.twitter.com/1.1/saved_searches/list.json"
    #f (saved-searches-list-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (saved-searches-show-:id #!key id) 
    "https://api.twitter.com/1.1/saved_searches/show/:id.json"
    #f (saved-searches-show-:id-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (geo-id-:place-id #!key place_id) 
    "https://api.twitter.com/1.1/geo/id/:place_id.json"
    #f (geo-id-:place-id-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (geo-reverse-geocode #!key lat long accuracy
				      granularity max_results callback) 
    "https://api.twitter.com/1.1/geo/reverse_geocode.json"
    #f (geo-reverse-geocode-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (geo-search #!key lat long query ip granularity
			     accuracy max_results contained_within
			     attribute:street_address)  
    "https://api.twitter.com/1.1/geo/search.json"
    #f (geo-search-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (trends-place #!key id exclude)
    "https://api.twitter.com/1.1/trends/place.json"
    #f (trends-place-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (trends-available)
    "https://api.twitter.com/1.1/trends/available.json"
    #f (trends-available-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (application-rate-limit-status #!key resources)
    "https://api.twitter.com/1.1/application/rate_limit_status.json"
    #f (application-rate-limit-status-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (help-configuration)
    "https://api.twitter.com/1.1/help/configuration.json"
    #f (help-configuration-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (help-languages)
    "https://api.twitter.com/1.1/help/languages.json"
    #f (help-languages-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (help-privacy)
    "https://api.twitter.com/1.1/help/privacy.json"
    #f (help-privacy-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (help-tos)
    "https://api.twitter.com/1.1/help/tos.json"
    #f (help-tos-reader) #f)

  ;; -------------------------------------------------------------------------
  (define-method (trends-closest #!key lat long)
    "https://api.twitter.com/1.1/trends/closest.json"
    #f (trends-closest-reader) #f)
  

  ) ;; end of module clucker

;;; clucker.scm ends here
