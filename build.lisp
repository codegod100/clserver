#!/usr/bin/env sbcl --script

;; Consolidated build script for Parenscript web servers
(load "~/quicklisp/setup.lisp")

;; Load Parenscript, cl-json, and djula via Quicklisp
(ql:quickload :parenscript)
(ql:quickload :cl-json)
(ql:quickload :djula)

;; Load our files directly
(load "package.lisp")
(load "worker.lisp")

(in-package :clserver)

(format t "Building Parenscript Cloudflare Worker...~%")

;; Build the worker
(build-all)

(format t "~%Build complete!~%")
(format t "Worker:           worker.js~%")
(format t "~%Deploy with:~%")
(format t "  wrangler dev     # Local development~%")
(format t "  wrangler deploy  # Deploy to Cloudflare~%")
(format t "~%Test routes:~%")
(format t "  /                # Home page~%")
(format t "  /user/alice      # User profile~%")
(format t "  /post/123        # Blog post~%")
(format t "  /api/stats       # API statistics~%")