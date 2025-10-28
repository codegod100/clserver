#!/usr/bin/env sbcl --script

;; Consolidated build script for Parenscript web servers
(load "~/quicklisp/setup.lisp")

;; Load Parenscript, cl-json, and djula via Quicklisp
(ql:quickload :parenscript)
(ql:quickload :cl-json)
(ql:quickload :djula)

;; Load our files directly
(load "package.lisp")
(load "servers.lisp")

(in-package :clserver)

(format t "Building Parenscript Web Server...~%")

;; Build the server
(build-all)

(format t "~%Build complete!~%")
(format t "Server:          node server.js~%")
(format t "~%Test with:~%")
(format t "  curl http://localhost:3000~%")
(format t "  curl http://localhost:3000/user/alice~%")
(format t "  curl http://localhost:3000/post/123~%")
(format t "  curl http://localhost:3000/api/stats~%")