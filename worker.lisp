(in-package #:clserver)

;; =============================================================================
;; MACRO SYSTEM FOR API GENERATION
;; =============================================================================

;; Global registry for API routes
(defvar *api-routes* (make-hash-table :test 'equal))

;; Registry of API definitions captured via DEFAPI
(defvar *api-definitions* '())

;; Macro to capture API definitions for later generation
(defmacro defapi (resource-name &body routes)
  "Capture API route specifications for later generation."
  (let ((processed-routes
          (mapcar (lambda (route)
                    (destructuring-bind (method path handler) route
                      (list :method (string-upcase (symbol-name method))
                            :path path
                            :handler handler)))
                  routes)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (push (list :resource ',resource-name :routes ',processed-routes)
             *api-definitions*)
       ',resource-name)))

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

;; =============================================================================
;; MAIN PARENSCRIPT CLOUDFLARE WORKER WITH ROUTING
;; =============================================================================

(defun generate-worker ()
  (let ((js-code (ps
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
                 
                 (defun send-error (status message)
                   (new ((@ self "Response") message
                             (create :status status
                                     :headers (create "content-type" "text/plain")))))
                 
                 (defun handle-root ()
                   (setf *visitor-count* (+ *visitor-count* 1))
                   (let ((html (+ "<!DOCTYPE html><html><head><title>Parenscript Worker</title></head><body>"
                                  "<h1>Welcome to Parenscript Cloudflare Worker</h1>"
                                  "<p>Visitor count: " *visitor-count* "</p>"
                                  "<p>Server: Parenscript Cloudflare Worker</p>"
                                  "</body></html>")))
                     (send-html html)))
                 
                 (defun handle-api-stats ()
                   (let ((json-response (+ "{\"visitor-count\":" *visitor-count* 
                                           ",\"server\":\"Parenscript Cloudflare Worker\""
                                           ",\"timestamp\":" (funcall (@ self "Date" now)) "}")))
                     (send-json json-response)))
                 
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
                 
                 ;; Simple API request handler
                 (defun handle-api-request (request)
                   (let* ((url (new ((@ self "URL") (@ request url))))
                          (pathname (@ url pathname))
                          (method (@ request method)))
                     (cond
                       ((and (= method "GET") (= pathname "/api/users"))
                        (let ((result (list-users request (create) nil)))
                          (send-json (funcall (@ self "JSON" stringify) result))))
                       ((and (= method "POST") (= pathname "/api/users"))
                        (chain (request.json)
                               (then (lambda (body)
                                       (let ((result (create-user request (create) body)))
                                         (send-json (funcall (@ self "JSON" stringify) result)))))))
                       ((and (= method "GET") (and (> (@ pathname length) 8) (= (funcall (@ pathname substring) 0 8) "/api/users/")))
                        (let ((user-id (funcall (@ pathname substring) 9))
                              (params (create :id user-id)))
                          (let ((result (get-user request params nil)))
                            (send-json (funcall (@ self "JSON" stringify) result)))))
                       ((and (= method "PUT") (and (> (@ pathname length) 8) (= (funcall (@ pathname substring) 0 8) "/api/users/")))
                        (let ((user-id (funcall (@ pathname substring) 9))
                              (params (create :id user-id)))
                          (chain (request.json)
                                 (then (lambda (body)
                                         (let ((result (update-user request params body)))
                                           (send-json (funcall (@ self "JSON" stringify) result))))))))
                       ((and (= method "DELETE") (and (> (@ pathname length) 8) (= (funcall (@ pathname substring) 0 8) "/api/users/")))
                        (let ((user-id (funcall (@ pathname substring) 9))
                              (params (create :id user-id)))
                          (let ((result (delete-user request params nil)))
                            (send-json (funcall (@ self "JSON" stringify) result)))))
                       (t (send-error 404 "API endpoint not found")))))
                 
                 (defun handle-request (request)
                   (let ((pathname (@ (new ((@ self "URL") (@ request url))) pathname)))
                     (cond
                       ((= pathname "/")
                        (handle-root))
                       ((and (> (@ pathname length) 4) (= (funcall (@ pathname substring) 0 4) "/api"))
                        (handle-api-request request))
                       ((= pathname "/api/stats")
                        (handle-api-stats))
                       (t
                        (send-error 404 "Not Found")))))
                 
                 ;; Cloudflare Worker event handler
                 (funcall (@ self "addEventListener") "fetch" (lambda (event)
                                                                (funcall (@ event "respondWith") (handle-request (@ event request))))))))
    (with-open-file (stream "worker.js" :direction :output :if-exists :supersede)
      (write-string js-code stream))
    (format t "Generated worker.js successfully!~%")))

;; Main build function
(defun build-all ()
  "Build the Cloudflare Worker"
  (generate-worker)
  (format t "~%Cloudflare Worker built successfully!~%"))