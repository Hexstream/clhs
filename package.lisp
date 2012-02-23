(in-package #:cl-user)

(defpackage #:clhs
  (:use #:cl)
  (:export #:*system-directory*
	   #:*hyperspec-relative-directory*

	   #:hyperspec-root
           #:install-clhs-use-local
           #:emacs-setup-form
	   #:print-emacs-setup-form))
