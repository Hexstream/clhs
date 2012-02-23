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

(defun %copy-file (source destination
                   &key (if-source-does-not-exist :error)
                   (if-destination-exists :error))
  (unless (and (eq if-destination-exists nil) (probe-file destination))
    (let (buffer)
      (with-open-file (in source
                          :direction :input
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist if-source-does-not-exist)
        (unless in (return-from %copy-file))
        (setf buffer (make-array (file-length in)
                                 :element-type '(unsigned-byte 8)))
        (read-sequence buffer in))
      (when buffer
        (with-open-file (out destination
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists if-destination-exists)
          (unless out (return-from %copy-file))
          (write-sequence buffer out)))
      t)))

(defun %default-clhs-use-local-directory ()
  (make-pathname
   :name nil :type nil
   :defaults (merge-pathnames (make-pathname
                               :directory '(:relative "quicklisp"))
                              (user-homedir-pathname))))

(defun %clhs-use-local (directory)
  (make-pathname :name "clhs-use-local" :type "el"
                 :defaults directory))

(defun install-clhs-use-local (&key if-exists
                               (verbose t)
                               (destination-directory
                                (%default-clhs-use-local-directory))
                               ensure-directories-exist-p)
  (when ensure-directories-exist-p
    (ensure-directories-exist destination-directory :verbose verbose))
  (%copy-file (%clhs-use-local *system-directory*)
              (%clhs-use-local destination-directory)
              :if-destination-exists if-exists))

(defun emacs-setup-form (&key (root (hyperspec-root))
                         (indirect-through-quicklisp-p t))
  (if indirect-through-quicklisp-p
      "(load (expand-file-name \"~/quicklisp/clhs-use-local.el\") t)"
      (format nil "(setq common-lisp-hyperspec-root~%      \"file:~A\")"
              (namestring root))))

(defun print-emacs-setup-form (&key (root (hyperspec-root))
                               ((:indirect-through-quicklisp-p indirectp) t))
  (when (and indirectp
             (not (probe-file (%clhs-use-local
                               (%default-clhs-use-local-directory)))))
    (format t "~
\~2&(Please run (clhs:install-clhs-use-local) in the (Common Lisp) REPL.
This will install clhs-use-local.el in your ~~/quicklisp/ directory.
The step below depends on this file.)~2%"))
  (format t "~2&Make Emacs evaluate this form to browse the CLHS locally:

~A


Use C-c C-d h make-instance RET to test if the change was successful.
If it was, then this will open your browser and the URL will begin with \"file:///\".

Put the form in your ~~/.emacs to persist the change for future sessions.

The README file has some further information.
\(Location: ~A)~2%"
	  (emacs-setup-form :root root
                            :indirect-through-quicklisp-p indirectp)
	  (make-pathname :name "README"
			 :defaults *system-directory*))
  (values))
