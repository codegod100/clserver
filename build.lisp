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

