(in-package #:cl-user)

(defpackage #:clhs
  (:use #:cl)
  (:export #:*system-directory*
	   #:*hyperspec-relative-directory*
           #:*quicklisp-directory*

           #:hyperspec-root
           #:clhs-use-local-status
           #:install-clhs-use-local
           #:emacs-setup-form
	   #:print-emacs-setup-form))
