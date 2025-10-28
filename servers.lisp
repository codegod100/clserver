(in-package #:clserver)

;; Main Parenscript Cloudflare Worker with routing
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
                                           ",\"timestamp\":" (funcall (@ Date now)) "}")))
                     (send-json json-response)))
                 
                 (defun handle-request (request)
                   (let ((pathname (@ (new ((@ self "URL") (@ request url))) pathname)))
                     (cond
                       ((= pathname "/")
                        (handle-root))
                       
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