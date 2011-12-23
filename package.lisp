(in-package #:cl-user)

(defpackage #:clhs
  (:use #:cl)
  (:export #:*system-directory*
	   #:*hyperspec-relative-directory*

	   #:hyperspec-root
	   #:emacs-setup-form
	   #:print-emacs-setup-form))
