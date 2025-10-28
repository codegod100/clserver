(in-package #:clserver)

;; =============================================================================
;; MACRO SYSTEM FOR API GENERATION
;; =============================================================================

;; Global registry for API routes
(defvar *api-routes* (make-hash-table :test 'equal))

;; Helper function to extract parameters from URL patterns
(defun extract-url-params (pattern path)
  "Extract parameters from URL pattern like /users/:id from actual path"
  (let ((pattern-parts (split-sequence #\/ pattern :remove-empty-subseqs t))
        (path-parts (split-sequence #\/ path :remove-empty-subseqs t))
        (params (make-hash-table :test 'equal)))
    (when (= (length pattern-parts) (length path-parts))
      (loop for pattern-part in pattern-parts
            for path-part in path-parts
            do (when (and (> (length pattern-part) 1)
                          (char= (char pattern-part 0) #\:))
                 (setf (gethash (subseq pattern-part 1) params) path-part)))
      params)))

;; Helper function to check if path matches pattern
(defun path-matches-pattern (pattern path)
  "Check if a path matches a URL pattern with parameters"
  (let ((pattern-parts (split-sequence #\/ pattern :remove-empty-subseqs t))
        (path-parts (split-sequence #\/ path :remove-empty-subseqs t)))
    (and (= (length pattern-parts) (length path-parts))
         (every (lambda (pattern-part path-part)
                  (or (and (> (length pattern-part) 1)
                           (char= (char pattern-part 0) #\:))
                      (string= pattern-part path-part)))
                pattern-parts path-parts))))

;; Macro to generate API endpoints
(defmacro defapi (resource-name &body routes)
  "Generate complete API endpoints for a resource
  
  Example:
  (defapi user
    (:get \"/users\" list-users)
    (:post \"/users\" create-user)
    (:get \"/users/:id\" get-user)
    (:put \"/users/:id\" update-user)
    (:delete \"/users/:id\" delete-user))"
  (let ((resource-symbol (intern (string-upcase resource-name)))
        (route-handlers '()))
    
    ;; Generate handler functions for each route
    (dolist (route routes)
      (let* ((method (first route))
             (path (second route))
             (handler-name (third route))
             (method-str (string-upcase method))
             (path-key (format nil "~a ~a" method-str path)))
        
        ;; Register the route
        (setf (gethash path-key *api-routes*) handler-name)
        
        ;; Generate handler function
        (push `(defun ,handler-name (request)
                 (let* ((url (new ((@ self "URL") (@ request url))))
                        (pathname (@ url pathname))
                        (params (extract-url-params ,path pathname))
                        (body (when (member ,method-str '("POST" "PUT" "PATCH") :test 'string=)
                                (chain (request.json) (then (lambda (json) json))))))
                   
                   ;; Call the actual handler function
                   (let ((result (funcall #',(intern (format nil "~a-~a" resource-symbol handler-name)) 
                                          request params body)))
                     (if (stringp result)
                         (send-json result)
                         (send-json (funcall (@ self "JSON" stringify) result))))))
              route-handlers)))
    
    ;; Generate the main API handler
    `(progn
       ,@route-handlers
       
       ;; Register API routes in the main router
       (defun register-api-routes ()
         (maphash (lambda (route handler)
                    (format t "Registered API route: ~a -> ~a~%" route handler))
                  *api-routes*))
       
       ;; Generate route matching logic
       (defun match-api-route (method path)
         (let ((route-key (format nil "~a ~a" method path)))
           (gethash route-key *api-routes*)))
       
       ;; Generate parameterized route matching
       (defun match-api-route-with-params (method path)
         (maphash (lambda (route-key handler)
                    (let ((route-parts (split-sequence #\Space route-key)))
                      (when (and (= (length route-parts) 2)
                                 (string= (first route-parts) method))
                        (let ((pattern (second route-parts)))
                          (when (path-matches-pattern pattern path)
                            (return-from match-api-route-with-params 
                              (values handler (extract-url-params pattern path))))))))
                  *api-routes*)
         nil))))

;; =============================================================================
;; SAMPLE API IMPLEMENTATIONS
;; =============================================================================

;; Example user resource implementation
(defapi user
  (:get "/users" list-users)
  (:post "/users" create-user)
  (:get "/users/:id" get-user)
  (:put "/users/:id" update-user)
  (:delete "/users/:id" delete-user))

;; User resource handler functions
(defun user-list-users (request params body)
  "List all users"
  (let ((users (list (create :id 1 :name "Alice" :email "alice@example.com")
                     (create :id 2 :name "Bob" :email "bob@example.com")
                     (create :id 3 :name "Charlie" :email "charlie@example.com"))))
    (create :users users :count (length users) :status "success")))

(defun user-create-user (request params body)
  "Create a new user"
  (if body
      (let ((new-user (create :id (+ 1 (random 1000))
                              :name (gethash "name" body)
                              :email (gethash "email" body)
                              :created-at (funcall (@ self "Date" now)))))
        (create :user new-user :status "created"))
      (create :error "Missing user data" :status "error")))

(defun user-get-user (request params body)
  "Get a specific user by ID"
  (let ((user-id (gethash "id" params)))
    (if user-id
        (let ((user (create :id (parse-integer user-id)
                            :name "Sample User"
                            :email "user@example.com"
                            :created-at (funcall (@ self "Date" now)))))
          (create :user user :status "success"))
        (create :error "User ID required" :status "error"))))

(defun user-update-user (request params body)
  "Update a user by ID"
  (let ((user-id (gethash "id" params)))
    (if (and user-id body)
        (let ((updated-user (create :id (parse-integer user-id)
                                    :name (gethash "name" body)
                                    :email (gethash "email" body)
                                    :updated-at (funcall (@ self "Date" now)))))
          (create :user updated-user :status "updated"))
        (create :error "User ID and data required" :status "error"))))

(defun user-delete-user (request params body)
  "Delete a user by ID"
  (let ((user-id (gethash "id" params)))
    (if user-id
        (create :message (format nil "User ~a deleted" user-id) :status "deleted")
        (create :error "User ID required" :status "error"))))

;; =============================================================================
;; MAIN PARENSCRIPT CLOUDFLARE WORKER WITH ROUTING
;; =============================================================================
(defun generate-worker ()
  (let ((js-code (eval `(ps
                 ;; Cloudflare Worker global state
                 (defvar *visitor-count* 0)
                 
                 (defun send-html (html)
                   (new ((@ self "Response") html
                             (create :status 200
                                     :headers (create "content-type" "text/html")))))
                 
                 (defun send-json (json-string)
                   (new ((@ self "Response") json-string
                             (create :status 200
                                     :headers (create "content-type" "application/json")))))
                 
                 (defun substitute-template (template placeholder value)
                   (let ((parts (funcall (@ template split) placeholder)))
                     (funcall (@ parts join) value)))
                 
                 (defun send-error (status message)
                   (new ((@ self "Response") message
                             (create :status status
                                     :headers (create "content-type" "text/plain")))))
                 
                 (defun load-template (template-name)
                   (funcall (@ self "ASSETS" fetch) (new ((@ self "Request") (+ "https://example.com/" template-name)))))
                 
                 (defun handle-root ()
                   (setf *visitor-count* (+ *visitor-count* 1))
                   (chain (load-template "home.html")
                          (then (lambda (template-response)
                                  (if (@ template-response ok)
                                      (chain (template-response.text)
                                             (then (lambda (template)
                                                     (let ((html (substitute-template template 
                                                                                    "{{visitor_count}}" 
                                                                                    (+ "" *visitor-count*))))
                                                       (send-html html)))))
                                      (send-error 500 "Template not found"))))))
                 
                 (defun handle-user (username)
                   (chain (load-template "user.html")
                          (then (lambda (template-response)
                                  (if (@ template-response ok)
                                      (chain (template-response.text)
                                             (then (lambda (template)
                                                     (let ((html (substitute-template template 
                                                                                    "{{username}}" 
                                                                                    username)))
                                                       (let ((html-with-avatar (substitute-template html 
                                                                                                   "{{username_initial}}" 
                                                                                                   (funcall (@ username char-at) 0))))
                                                         (send-html html-with-avatar))))))
                                      (send-error 500 "Template not found"))))))
                 
                 (defun handle-post (post-id)
                   (chain (load-template "post.html")
                          (then (lambda (template-response)
                                  (if (@ template-response ok)
                                      (chain (template-response.text)
                                             (then (lambda (template)
                                                     (let ((html (substitute-template template 
                                                                                    "{{post_id}}" 
                                                                                    post-id)))
                                                       (send-html html)))))
                                      (send-error 500 "Template not found"))))))
                 
                 (defun handle-api-stats ()
                   (let ((json-response (+ "{\"visitor-count\":" *visitor-count* 
                                           ",\"server\":\"Parenscript Cloudflare Worker\""
                                           ",\"timestamp\":" (funcall (@ self "Date" now)) "}")))
                     (send-json json-response)))
                 
                 ;; Simple API route registry
                 (defvar *api-routes* (create))
                 
                 ;; Initialize API routes
                 (defun init-api-routes ()
                   (setf (@ *api-routes* "GET /users") "list-users")
                   (setf (@ *api-routes* "POST /users") "create-user")
                   (setf (@ *api-routes* "GET /users/:id") "get-user")
                   (setf (@ *api-routes* "PUT /users/:id") "update-user")
                   (setf (@ *api-routes* "DELETE /users/:id") "delete-user"))
                 
                 ;; Simple API handler functions
                 (defun list-users (request params body)
                   (let ((users (list (create :id 1 :name "Alice" :email "alice@example.com")
                                      (create :id 2 :name "Bob" :email "bob@example.com")
                                      (create :id 3 :name "Charlie" :email "charlie@example.com"))))
                     (create :users users :count (@ users length) :status "success")))
                 
                 (defun create-user (request params body)
                   (if body
                       (let ((new-user (create :id (+ 1 (funcall (@ self "Math" floor) 
                                                                  (* (funcall (@ self "Math" random)) 1000)))
                                               :name (@ body "name")
                                               :email (@ body "email")
                                               :created-at (funcall (@ self "Date" now)))))
                         (create :user new-user :status "created"))
                       (create :error "Missing user data" :status "error")))
                 
                 (defun get-user (request params body)
                   (let ((user-id (@ params "id")))
                     (if user-id
                         (let ((user (create :id (funcall (@ self "parseInt") user-id)
                                             :name "Sample User"
                                             :email "user@example.com"
                                             :created-at (funcall (@ self "Date" now)))))
                           (create :user user :status "success"))
                         (create :error "User ID required" :status "error"))))
                 
                 (defun update-user (request params body)
                   (let ((user-id (@ params "id")))
                     (if (and user-id body)
                         (let ((updated-user (create :id (funcall (@ self "parseInt") user-id)
                                                     :name (@ body "name")
                                                     :email (@ body "email")
                                                     :updated-at (funcall (@ self "Date" now)))))
                           (create :user updated-user :status "updated"))
                         (create :error "User ID and data required" :status "error"))))
                 
                 (defun delete-user (request params body)
                   (let ((user-id (@ params "id")))
                     (if user-id
                         (create :message (+ "User " user-id " deleted") :status "deleted")
                         (create :error "User ID required" :status "error"))))
                 
                 ;; API handler functions
                 (defun list-users (request params body)
                   (let ((users (list (create :id 1 :name "Alice" :email "alice@example.com")
                                      (create :id 2 :name "Bob" :email "bob@example.com")
                                      (create :id 3 :name "Charlie" :email "charlie@example.com"))))
                     (create :users users :count (@ users length) :status "success")))
                 
                 (defun create-user (request params body)
                   (if body
                       (let ((new-user (create :id (+ 1 (funcall (@ self "Math" floor) 
                                                                  (* (funcall (@ self "Math" random)) 1000)))
                                               :name (@ body "name")
                                               :email (@ body "email")
                                               :created-at (funcall (@ self "Date" now)))))
                         (create :user new-user :status "created"))
                       (create :error "Missing user data" :status "error")))
                 
                 (defun get-user (request params body)
                   (let ((user-id (@ params "id")))
                     (if user-id
                         (let ((user (create :id (funcall (@ self "parseInt") user-id)
                                             :name "Sample User"
                                             :email "user@example.com"
                                             :created-at (funcall (@ self "Date" now)))))
                           (create :user user :status "success"))
                         (create :error "User ID required" :status "error"))))
                 
                 (defun update-user (request params body)
                   (let ((user-id (@ params "id")))
                     (if (and user-id body)
                         (let ((updated-user (create :id (funcall (@ self "parseInt") user-id)
                                                     :name (@ body "name")
                                                     :email (@ body "email")
                                                     :updated-at (funcall (@ self "Date" now)))))
                           (create :user updated-user :status "updated"))
                         (create :error "User ID and data required" :status "error"))))
                 
                 (defun delete-user (request params body)
                   (let ((user-id (@ params "id")))
                     (if user-id
                         (create :message (+ "User " user-id " deleted") :status "deleted")
                         (create :error "User ID required" :status "error"))))
                 
                 ;; Simple API request handler
                 (defun handle-api-request (request)
                   (let* ((url (new ((@ self "URL") (@ request url))))
                          (pathname (@ url pathname))
                          (method (@ request method))
                          (route-key (+ method " " pathname))
                          (handler (@ *api-routes* route-key)))
                     (if handler
                         (let ((result (funcall (@ self handler) request (create) 
                                                (when (member method '("POST" "PUT" "PATCH"))
                                                  (chain (request.json) (then (lambda (json) json)))))))
                           (if (stringp result)
                               (send-json result)
                               (send-json (funcall (@ self "JSON" stringify) result))))
                         (send-error 404 "API endpoint not found"))))
                 
                 (defun handle-request (request)
                   (let ((pathname (@ (new ((@ self "URL") (@ request url))) pathname)))
                     (cond
                       ((= pathname "/")
                        (handle-root))
                       
                       ((and (> (@ pathname length) 4) (= (funcall (@ pathname substring) 0 4) "/api"))
                        (handle-api-request request))
                       
                       ((and (> (@ pathname length) 6) (= (funcall (@ pathname substring) 0 6) "/user/"))
                        (let ((username (funcall (@ pathname substring) 6)))
                          (handle-user username)))
                       
                       ((and (> (@ pathname length) 6) (= (funcall (@ pathname substring) 0 6) "/post/"))
                        (let ((post-id (funcall (@ pathname substring) 6)))
                          (handle-post post-id)))
                       
                       ((= pathname "/api/stats")
                        (handle-api-stats))
                       
                       (t
                        (send-error 404 "Not Found")))))
                 
                 ;; Initialize API routes on startup
                 (init-api-routes)
                 
                 ;; Cloudflare Worker event handler
                 (funcall (@ self "addEventListener") "fetch" (lambda (event)
                                                                (funcall (@ event "respondWith") (handle-request (@ event request)))))))))
    (with-open-file (stream "worker.js" :direction :output :if-exists :supersede)
      (write-string js-code stream))
    (format t "Generated worker.js successfully!~%")))

;; Main build function
(defun build-all ()
  "Build the Cloudflare Worker"
  (generate-worker)
  (format t "~%Cloudflare Worker built successfully!~%"))