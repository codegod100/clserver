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



(defparameter +worker-form-visitor-count+
  '(defvar *visitor-count* 0)
)
(defparameter +worker-form-send-html+
  '(defun send-html (html)
            (new ((@ self "Response") html
                       (create :status 200
                               :headers (create "content-type" "text/html")))))
)
(defparameter +worker-form-send-json+
  '(defun send-json (json-string)
            (new ((@ self "Response") json-string
                       (create :status 200
                               :headers (create "content-type" "application/json")))))
)
(defparameter +worker-form-send-error+
  '(defun send-error (status message)
            (new ((@ self "Response") message
                       (create :status status
                               :headers (create "content-type" "text/plain")))))
)
(defparameter +worker-form-render-api-demo-page+
  '(defun render-api-demo-page (visitor-count origin)
            (let ((data (create :visitor-count visitor-count
                                :origin origin
                                :server-name "Parenscript Cloudflare Worker")))
              (render-template "api-demo.html" data)))
)
(defparameter +worker-form-render-template+
  '(defun render-template (template-name data)
            "Render a template with the given data"
            (let ((template (load-template template-name)))
              (substitute-template-variables template data)))
)
(defparameter +worker-form-load-template+
  '(defun load-template (template-name)
            "Load a template by name - for now return hardcoded template"
            (get-template template-name))
)
(defparameter +worker-form-get-template+
  '(defun get-template (template-name)
            "Get template content by name"
            (cond
              ((= template-name "api-demo.html")
               (get-api-demo-template))
              (t "<h1>Template not found</h1>")))
)
(defparameter +worker-form-get-api-demo-template+
  '(defun get-api-demo-template ()
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
)
(defparameter +worker-form-substitute-template-variables+
  '(defun substitute-template-variables (template data)
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
)
(defparameter +worker-form-get-api-demo-styles+
  '(defun get-api-demo-styles ()
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
               ".example { background: #f1f3f4; padding: 10px; border-radius: 3px; margin-top: 10px; font-family: monospace; font-size: 12px; }"
               "#ajax-result { background: #0b1021; color: #e3e7ff; padding: 12px; border-radius: 6px; font-family: monospace; white-space: pre-wrap; overflow-x: auto; }"
               ".btn { display: inline-block; margin: 6px 8px 6px 0; padding: 8px 12px; border-radius: 6px; border: 0; cursor: pointer; font-weight: 600; }"
               ".btn.get { background: #d4edda; color: #155724; }"
               ".btn.post { background: #cce5ff; color: #004085; }"
               ".btn.put { background: #fff3cd; color: #856404; }"
               ".btn.delete { background: #f8d7da; color: #721c24; }"
               ".hint { color: #666; font-size: 12px; margin-top: 6px; }"))
)
(defparameter +worker-form-render-stats-section+
  '(defun render-stats-section (data)
            "Render the server stats section"
            (+ "<div class=\"stats\">"
               "<h3>📊 Server Stats</h3>"
               "<p><strong>Visitor Count:</strong> " (@ data "visitor-count") "</p>"
               "<p><strong>Server:</strong> " (@ data "server-name") "</p>"
               "<p><strong>Status:</strong> <span style=\"color: green;\">✅ Online</span></p>"
               "</div>"))
)
(defparameter +worker-form-render-api-endpoints-section+
  '(defun render-api-endpoints-section (data)
            "Render the API endpoints section with inline AJAX demo buttons"
            (let ((origin (@ data "origin")))
              (+ "<div class=\"api-section\">"
                 "<h2>🔗 Available API Endpoints</h2>"
                 (render-api-endpoint "GET" "/api/stats" "Get server statistics and visitor count" (+ "curl " origin "/api/stats") origin "")
                 (render-api-endpoint "GET" "/api/work" "Process work and return timing information" (+ "curl " origin "/api/work") origin "")
                 (render-api-endpoint "GET" "/api/users" "List all users" (+ "curl " origin "/api/users") origin "")
                 (render-api-endpoint "POST" "/api/users" "Create a new user" (+ "curl -X POST " origin "/api/users -H \"Content-Type: application/json\" -d '{\"name\":\"John Doe\",\"email\":\"john@example.com\"}'") origin "{name:'John Doe',email:'john@example.com'}")
                 (render-api-endpoint "GET" "/api/users/:id" "Get a specific user by ID" (+ "curl " origin "/api/users/1") origin "")
                 (render-api-endpoint "PUT" "/api/users/:id" "Update a user by ID" (+ "curl -X PUT " origin "/api/users/1 -H \"Content-Type: application/json\" -d '{\"name\":\"Jane Doe\",\"email\":\"jane@example.com\"}'") origin "{name:'Jane Doe',email:'jane@example.com'}")
                 (render-api-endpoint "DELETE" "/api/users/:id" "Delete a user by ID" (+ "curl -X DELETE " origin "/api/users/1") origin "")
                 "<h3>Response</h3>"
                 "<div id=\"ajax-result\" class=\"example\">(no request yet)</div>"
                 "<script>"
                 "async function demoFetch(method, url, body, targetId){"
                 "  try {"
                 "    const opts = { method, headers: {} };"
                 "    if (body && Object.keys(body).length) { opts.headers['Content-Type'] = 'application/json'; opts.body = JSON.stringify(body); }"
                 "    console.log('Fetching:', method, url, body || '');"
                 "    const res = await fetch(url, opts);"
                 "    const text = await res.text();"
                 "    let json; try { json = JSON.parse(text); } catch(e) { json = { raw: text }; }"
                 "    const pretty = JSON.stringify(json, null, 2);"
                 "    const el = document.getElementById(targetId);"
                 "    if (el) { el.style.display = 'block'; el.textContent = pretty; }"
                 "  } catch (e) {"
                 "    const el = document.getElementById(targetId); if (el) { el.style.display = 'block'; el.textContent = 'Error: ' + (e && e.message ? e.message : e); }"
                 "  }"
                 "}"
                 "</script>"
                 "</div>")))
)
(defparameter +worker-form-render-api-endpoint+
  '(defun render-api-endpoint (method path description example origin body-js)
            "Render a single API endpoint with an inline AJAX demo button and its own response container"
            (let ((method-lower (funcall (@ method "toLowerCase")))
                  (full-url (+ origin path))
                  (body-arg (if (> (@ body-js length) 0) body-js "null"))
                  (sanitized (funcall (@ (funcall (@ (funcall (@ path "toLowerCase")) replace) (new ((@ self "RegExp") "[^a-z0-9]+" "g")) "-") replace) (new ((@ self "RegExp") "^-|-$" "g")) ""))
                  (resp-id ""))
              (setf resp-id (+ "ajax-result-" method-lower "-" sanitized))
              (+ "<div class=\"api-endpoint\">"
                 "<span class=\"method " method-lower "\">" method "</span>"
                 "<span class=\"url\">" path "</span>"
                 "<div class=\"description\">" description "</div>"
                 "<div class=\"example\">" example "</div>"
                 "<div><button class=\"btn " method-lower "\" onclick=\"demoFetch('" method "','" full-url "', " body-arg ", '" resp-id "')\">Try</button></div>"
                 "<div class=\"example\" id=\"" resp-id "\" style=\"display:none\"></div>"
                 "</div>")))
)
(defparameter +worker-form-render-try-it-out-section+
  '(defun render-try-it-out-section ()
            "Render the try it out section"
            (+ "<div class=\"api-section\">"
               "<h2>🧪 Try It Out</h2>"
               "<p>Use the curl commands above to test the API endpoints, or visit:</p>"
               "<ul>"
               "<li><a href=\"/api/stats\" target=\"_blank\">/api/stats</a> - Server statistics</li>"
               "<li><a href=\"/api/work\" target=\"_blank\">/api/work</a> - Process work with timing</li>"
               "<li><a href=\"/api/users\" target=\"_blank\">/api/users</a> - List users</li>"
               "</ul>"
               "</div>"))
)
(defparameter +worker-form-render-about-section+
  '(defun render-about-section ()
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
)
(defparameter +worker-form-handle-root+
  '(defun handle-root (request)
            (setf *visitor-count* (+ *visitor-count* 1))
            (let* ((url (new ((@ self "URL") (@ request url))))
                   (origin (@ url origin)))
              (let ((html (render-api-demo-page *visitor-count* origin)))
                (send-html html))))
)
(defparameter +worker-form-handle-api-stats+
  '(defun handle-api-stats ()
            (let ((json-response (+ "{\"visitor-count\":" *visitor-count*
                                    ",\"server\":\"Parenscript Cloudflare Worker\""
                                    ",\"timestamp\":" (funcall (@ self "Date" now)) "}")))
              (send-json json-response)))
)
(defparameter +worker-form-list-users+
  '(defun list-users (request params body)
            (let ((users (list (create :id 1 :name "Alice" :email "alice@example.com")
                               (create :id 2 :name "Bob" :email "bob@example.com")
                               (create :id 3 :name "Charlie" :email "charlie@example.com"))))
              (create :users users :count (@ users length) :status "success")))
)
(defparameter +worker-form-create-user+
  '(defun create-user (request params body)
            (if body
                (let ((new-user (create :id (+ 1 (funcall (@ self "Math" floor)
                                                          (* (funcall (@ self "Math" random)) 1000)))
                                        :name (@ body "name")
                                        :email (@ body "email")
                                        :created-at (funcall (@ self "Date" now)))))
                  (create :user new-user :status "created"))
                (create :error "Missing user data" :status "error")))
)
(defparameter +worker-form-get-user+
  '(defun get-user (request params body)
            (let ((user-id (@ params "id")))
              (if user-id
                  (let ((user (create :id (funcall (@ self "parseInt") user-id)
                                      :name "Sample User"
                                      :email "user@example.com"
                                      :created-at (funcall (@ self "Date" now)))))
                    (create :user user :status "success"))
                  (create :error "User ID required" :status "error"))))
)
(defparameter +worker-form-update-user+
  '(defun update-user (request params body)
            (let ((user-id (@ params "id")))
              (if (and user-id body)
                  (let ((updated-user (create :id (funcall (@ self "parseInt") user-id)
                                              :name (@ body "name")
                                              :email (@ body "email")
                                              :updated-at (funcall (@ self "Date" now)))))
                    (create :user updated-user :status "updated"))
                  (create :error "User ID and data required" :status "error"))))
)
(defparameter +worker-form-delete-user+
  '(defun delete-user (request params body)
            (let ((user-id (@ params "id")))
              (if user-id
                  (create :message (+ "User " user-id " deleted") :status "deleted")
                  (create :error "User ID required" :status "error"))))
)
(defparameter +worker-form-process-work+
  '(defun process-work (request params body)
            "Process some work and return timing information"
            (let* ((start-time (funcall (@ self "Date" now)))
                   (work-result (simulate-work))
                   (end-time (funcall (@ self "Date" now)))
                   (duration (- end-time start-time)))
              (create :result work-result
                      :duration-ms duration
                      :start-time start-time
                      :end-time end-time
                      :status "completed")))
)
(defparameter +worker-form-simulate-work+
  '(defun simulate-work ()
            "Simulate some CPU-intensive work"
            (let ((result 0))
              (dotimes (i 1000000)
                (setf result (+ result i)))
              result))
)
(defparameter +worker-form-handle-api-request+
  '(defun handle-api-request (request)
            (let* ((url (new ((@ self "URL") (@ request url))))
                   (pathname (@ url pathname))
                   (method (@ request method)))
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
                ((and (= method "GET") (= pathname "/api/work"))
                 (let ((result (process-work request (create) nil)))
                   (send-json (funcall (@ self "JSON" stringify) result))))
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
)
(defparameter +worker-form-handle-request+
  '(defun handle-request (request)
            (let ((pathname (@ (new ((@ self "URL") (@ request url))) pathname)))
              (cond
                ((= pathname "/")
                 (handle-root request))
                ((= pathname "/api/stats")
                 (handle-api-stats))
                ((= pathname "/api/work")
                 (let ((result (process-work request (create) nil)))
                   (send-json (funcall (@ self "JSON" stringify) result))))
                ((and (> (@ pathname length) 4) (= (funcall (@ pathname substring) 0 4) "/api"))
                 (handle-api-request request))
                (t
                 (send-error 404 "Not Found")))))
)


(defparameter +worker-runtime-definitions+
  (list
    +worker-form-visitor-count+
    +worker-form-send-html+
    +worker-form-send-json+
    +worker-form-send-error+
    +worker-form-render-api-demo-page+
    +worker-form-render-template+
    +worker-form-load-template+
    +worker-form-get-template+
    +worker-form-get-api-demo-template+
    +worker-form-substitute-template-variables+
    +worker-form-get-api-demo-styles+
    +worker-form-render-stats-section+
    +worker-form-render-api-endpoints-section+
    +worker-form-render-api-endpoint+
    +worker-form-render-try-it-out-section+
    +worker-form-render-about-section+
    +worker-form-handle-root+
    +worker-form-handle-api-stats+
    +worker-form-list-users+
    +worker-form-create-user+
    +worker-form-get-user+
    +worker-form-update-user+
    +worker-form-delete-user+
    +worker-form-process-work+
    +worker-form-simulate-work+
    +worker-form-handle-api-request+
    +worker-form-handle-request+
  )
  "Parenscript forms that define the Cloudflare Worker runtime.")
(defun generate-worker ()
  (let* ((body (append +worker-runtime-definitions+
                       (list '(funcall (@ self "addEventListener") "fetch"
                                       (lambda (event)
                                         (funcall (@ event "respondWith")
                                                  (handle-request (@ event request))))))))
         (js-code (ps `(progn ,@body))))
    (with-open-file (stream "worker.js" :direction :output :if-exists :supersede)
      (write-string js-code stream))
    (format t "Generated worker.js successfully!~%")))

(defun build-all ()
  "Build the Cloudflare Worker"
  (generate-worker)
  (format t "~%Cloudflare Worker built successfully!~%"))