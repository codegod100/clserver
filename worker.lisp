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
                 
                 ;; Template functions for HTML generation
                 (defun render-api-demo-page (visitor-count origin)
                   (let ((data (create :visitor-count visitor-count
                                       :origin origin
                                       :server-name "Parenscript Cloudflare Worker")))
                     (render-template "api-demo.html" data)))
                 
                 (defun render-template (template-name data)
                   "Render a template with the given data"
                   (let ((template (load-template template-name)))
                     (substitute-template-variables template data)))
                 
                 (defun load-template (template-name)
                   "Load a template by name - for now return hardcoded template"
                   (get-template template-name))
                 
                 (defun get-template (template-name)
                   "Get template content by name"
                   (cond
                     ((= template-name "api-demo.html")
                      (get-api-demo-template))
                     (t "<h1>Template not found</h1>")))
                 
                 (defun get-api-demo-template ()
                   "Get the API demo page template"
                   (+ "<!DOCTYPE html>"
                      "<html><head>"
                      "<meta charset=\"utf-8\">"
                      "<title>{{title}}</title>"
                      "<style>{{styles}}</style>"
                      "</head><body>"
                      "<div class=\"container\">"
                      "<h1>{{header}}</h1>"
                      "{{stats-section}}"
                      "{{api-endpoints-section}}"
                      "{{try-it-out-section}}"
                      "{{about-section}}"
                      "</div>"
                      "</body></html>"))
                 
                 (defun substitute-template-variables (template data)
                   "Substitute template variables with data"
                   (let ((result template))
                     (setf result (funcall (@ result replace) "{{title}}" "Parenscript Cloudflare Worker Demo"))
                     (setf result (funcall (@ result replace) "{{styles}}" (get-api-demo-styles)))
                     (setf result (funcall (@ result replace) "{{header}}" "🚀 Parenscript Cloudflare Worker Demo"))
                     (setf result (funcall (@ result replace) "{{stats-section}}" (render-stats-section data)))
                     (setf result (funcall (@ result replace) "{{api-endpoints-section}}" (render-api-endpoints-section data)))
                     (setf result (funcall (@ result replace) "{{try-it-out-section}}" (render-try-it-out-section)))
                     (setf result (funcall (@ result replace) "{{about-section}}" (render-about-section)))
                     result))
                 
                 (defun get-api-demo-styles ()
                   "Get CSS styles for the API demo page"
                   (+ "body { font-family: Arial, sans-serif; margin: 40px; background: #f5f5f5; }"
                      ".container { max-width: 800px; margin: 0 auto; background: white; padding: 30px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }"
                      "h1 { color: #333; border-bottom: 3px solid #007acc; padding-bottom: 10px; }"
                      ".stats { background: #e8f4fd; padding: 15px; border-radius: 5px; margin: 20px 0; }"
                      ".api-section { margin: 30px 0; }"
                      ".api-endpoint { background: #f8f9fa; border-left: 4px solid #007acc; padding: 15px; margin: 10px 0; border-radius: 0 5px 5px 0; }"
                      ".method { display: inline-block; padding: 4px 8px; border-radius: 3px; font-weight: bold; margin-right: 10px; }"
                      ".get { background: #d4edda; color: #155724; }"
                      ".post { background: #cce5ff; color: #004085; }"
                      ".put { background: #fff3cd; color: #856404; }"
                      ".delete { background: #f8d7da; color: #721c24; }"
                      ".url { font-family: monospace; color: #007acc; }"
                      ".description { color: #666; margin-top: 5px; }"
                      ".example { background: #f1f3f4; padding: 10px; border-radius: 3px; margin-top: 10px; font-family: monospace; font-size: 12px; }"))
                 
                 (defun render-stats-section (data)
                   "Render the server stats section"
                   (+ "<div class=\"stats\">"
                      "<h3>📊 Server Stats</h3>"
                      "<p><strong>Visitor Count:</strong> " (@ data "visitor-count") "</p>"
                      "<p><strong>Server:</strong> " (@ data "server-name") "</p>"
                      "<p><strong>Status:</strong> <span style=\"color: green;\">✅ Online</span></p>"
                      "</div>"))
                 
                 (defun render-api-endpoints-section (data)
                   "Render the API endpoints section"
                   (let ((origin (@ data "origin")))
                     (+ "<div class=\"api-section\">"
                        "<h2>🔗 Available API Endpoints</h2>"
                        (render-api-endpoint "GET" "/api/stats" "Get server statistics and visitor count" (+ "curl " origin "/api/stats"))
                        (render-api-endpoint "GET" "/api/users" "List all users" (+ "curl " origin "/api/users"))
                        (render-api-endpoint "POST" "/api/users" "Create a new user" (+ "curl -X POST " origin "/api/users -H \"Content-Type: application/json\" -d '{\"name\":\"John Doe\",\"email\":\"john@example.com\"}'"))
                        (render-api-endpoint "GET" "/api/users/:id" "Get a specific user by ID" (+ "curl " origin "/api/users/1"))
                        (render-api-endpoint "PUT" "/api/users/:id" "Update a user by ID" (+ "curl -X PUT " origin "/api/users/1 -H \"Content-Type: application/json\" -d '{\"name\":\"Jane Doe\",\"email\":\"jane@example.com\"}'"))
                        (render-api-endpoint "DELETE" "/api/users/:id" "Delete a user by ID" (+ "curl -X DELETE " origin "/api/users/1"))
                        "</div>")))
                 
                 (defun render-api-endpoint (method path description example)
                   "Render a single API endpoint"
                   (let ((method-lower (funcall (@ method "toLowerCase"))))
                     (+ "<div class=\"api-endpoint\">"
                        "<span class=\"method " method-lower "\">" method "</span>"
                        "<span class=\"url\">" path "</span>"
                        "<div class=\"description\">" description "</div>"
                        "<div class=\"example\">" example "</div>"
                        "</div>")))
                 
                 (defun render-try-it-out-section ()
                   "Render the try it out section"
                   (+ "<div class=\"api-section\">"
                      "<h2>🧪 Try It Out</h2>"
                      "<p>Use the curl commands above to test the API endpoints, or visit:</p>"
                      "<ul>"
                      "<li><a href=\"/api/stats\" target=\"_blank\">/api/stats</a> - Server statistics</li>"
                      "<li><a href=\"/api/users\" target=\"_blank\">/api/users</a> - List users</li>"
                      "</ul>"
                      "</div>"))
                 
                 (defun render-about-section ()
                   "Render the about section"
                   (+ "<div class=\"api-section\">"
                      "<h2>💡 About This Demo</h2>"
                      "<p>This is a <strong>Parenscript</strong> generated Cloudflare Worker that demonstrates:</p>"
                      "<ul>"
                      "<li>✅ RESTful API endpoints</li>"
                      "<li>✅ JSON request/response handling</li>"
                      "<li>✅ URL parameter extraction</li>"
                      "<li>✅ HTTP method routing</li>"
                      "<li>✅ Error handling and status codes</li>"
                      "<li>✅ Visitor tracking</li>"
                      "</ul>"
                      "<p><em>Built with Common Lisp and Parenscript</em></p>"
                      "</div>"))
                 
                 (defun handle-root (request)
                   (setf *visitor-count* (+ *visitor-count* 1))
                   (let* ((url (new ((@ self "URL") (@ request url))))
                          (origin (@ url origin)))
                     (let ((html (render-api-demo-page *visitor-count* origin)))
                       (send-html html))))
                 
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
                     ;; Debug logging
                     (funcall (@ self "console" log) "=== API REQUEST DEBUG ===")
                     (funcall (@ self "console" log) (+ "Method: " method))
                     (funcall (@ self "console" log) (+ "Pathname: " pathname))
                     (funcall (@ self "console" log) (+ "Pathname length: " (@ pathname length)))
                     (funcall (@ self "console" log) (+ "Pathname substring(0,11): " (funcall (@ pathname substring) 0 11)))
                     (funcall (@ self "console" log) (+ "Is GET: " (= method "GET")))
                     (funcall (@ self "console" log) (+ "Length > 10: " (> (@ pathname length) 10)))
                     (funcall (@ self "console" log) (+ "Substring equals '/api/users/': " (= (funcall (@ pathname substring) 0 11) "/api/users/")))
                     (funcall (@ self "console" log) (+ "Full condition: " (and (= method "GET") (and (> (@ pathname length) 10) (= (funcall (@ pathname substring) 0 11) "/api/users/")))))
                         (cond
                       ((and (= method "GET") (= pathname "/api/users"))
                            (let ((result (list-users request (create) nil)))
                          (send-json (funcall (@ self "JSON" stringify) result))))
                       ((and (= method "POST") (= pathname "/api/users"))
                        (chain (request.json)
                               (then (lambda (body)
                                       (let ((result (create-user request (create) body)))
                                         (send-json (funcall (@ self "JSON" stringify) result)))))))
                       ((and (= method "GET") (and (> (@ pathname length) 10) (= (funcall (@ pathname substring) 0 11) "/api/users/")))
                        (progn
                          (funcall (@ self "console" log) "Matched GET /api/users/ route")
                          (let* ((user-id (funcall (@ pathname substring) 11))
                                 (params (create :id user-id))
                                 (result (get-user request params nil)))
                            (funcall (@ self "console" log) (+ "User ID: " user-id))
                                  (send-json (funcall (@ self "JSON" stringify) result)))))
                       ((and (= method "PUT") (and (> (@ pathname length) 10) (= (funcall (@ pathname substring) 0 11) "/api/users/")))
                        (let* ((user-id (funcall (@ pathname substring) 11))
                               (params (create :id user-id)))
                            (chain (request.json)
                                   (then (lambda (body)
                                           (let ((result (update-user request params body)))
                                                 (send-json (funcall (@ self "JSON" stringify) result))))))))
                       ((and (= method "DELETE") (and (> (@ pathname length) 10) (= (funcall (@ pathname substring) 0 11) "/api/users/")))
                        (let* ((user-id (funcall (@ pathname substring) 11))
                               (params (create :id user-id))
                               (result (delete-user request params nil)))
                          (send-json (funcall (@ self "JSON" stringify) result))))
                       (t (send-error 404 "API endpoint not found")))))
                 
                 (defun handle-request (request)
                   (let ((pathname (@ (new ((@ self "URL") (@ request url))) pathname)))
                     (cond
                       ((= pathname "/")
                        (handle-root request))
                       ((= pathname "/api/stats")
                        (handle-api-stats))
                       ((and (> (@ pathname length) 4) (= (funcall (@ pathname substring) 0 4) "/api"))
                        (handle-api-request request))
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