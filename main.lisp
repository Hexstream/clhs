(in-package #:clhs)

(defparameter *system-directory*
  (make-pathname :name nil :type nil
		 :defaults #.(or *compile-file-truename* *load-truename*)))

(defparameter *hyperspec-relative-directory*
  '("HyperSpec-7-0" "HyperSpec"))

(defun hyperspec-root (&key (system-directory *system-directory*)
		       (relative-directory *hyperspec-relative-directory*))
  (merge-pathnames
   (make-pathname :directory `(:relative ,@relative-directory))
   system-directory))

(defun emacs-setup-form (&key (root (hyperspec-root)))
  (format nil "(setq common-lisp-hyperspec-root~%      \"file:~A\")"
	  (namestring root)))

(defun print-emacs-setup-form (&key (root (hyperspec-root)))
  (format t "~2&~A

Make Emacs evaluate the above form to browse the CLHS locally.

Use C-c C-d h make-instance RET to test if the change was successful.
If it was, then this will open your browser and the URL will begin with \"file:///\".

The README file has some further information.
\(Location: ~A)~2%"
	  (emacs-setup-form :root root)
	  (make-pathname :name "README"
			 :defaults *system-directory*))
  (values))
