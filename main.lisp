(in-package #:clhs)

(defparameter *system-directory*
  (make-pathname :name nil :type nil
		 :defaults #.(or *compile-file-truename* *load-truename*)))

(defparameter *hyperspec-relative-directory*
  '("HyperSpec-7-0" "HyperSpec"))

(defun %try-getting-authoritative-quicklisp-directory ()
  (let ((ql (find-package '#:quicklisp)))
    (and ql
         (let ((home-symbol (find-symbol (string '#:*quicklisp-home*) ql)))
           (and home-symbol
                (boundp home-symbol)
                (symbol-value home-symbol))))))

(defvar *quicklisp-directory*
  (or (%try-getting-authoritative-quicklisp-directory)
      (make-pathname
       :name nil :type nil
       :defaults (merge-pathnames (make-pathname
                                   :directory '(:relative "quicklisp"))
                                  (user-homedir-pathname)))))

(defun hyperspec-root (&key (system-directory *system-directory*)
		       (relative-directory *hyperspec-relative-directory*))
  (merge-pathnames
   (make-pathname :directory `(:relative ,@relative-directory))
   system-directory))

(defun %probe-file (pathname)
  #-clisp
  (probe-file pathname)
  #+clisp
  (ext:probe-pathname pathname))

(defun %copy-file (source destination
                   &key (if-source-does-not-exist :error)
                   (if-destination-exists :error))
  (unless (and (eq if-destination-exists nil) (%probe-file destination))
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

(defun %clhs-use-local (directory)
  (make-pathname :name "clhs-use-local" :type "el"
                 :defaults directory))

(defun %clhs-use-local-version (clhs-use-local)
  (when clhs-use-local
    (with-open-file (stream clhs-use-local)
      (let ((line (read-line stream)))
        (subseq line (1+ (position #\Space line :from-end t)))))))

(defun clhs-use-local-status (&key (quicklisp-directory *quicklisp-directory*))
  (let* ((bundled
          (%probe-file (%clhs-use-local *system-directory*)))
         (expected-installed
          (%clhs-use-local quicklisp-directory))
         (installed
          (%probe-file expected-installed))
         (bundled-version (%clhs-use-local-version bundled))
         (installed-version (%clhs-use-local-version installed)))
    (values (cond ((not installed)
                   :not-installed)
                  ((string/= installed-version bundled-version)
                   :version-mismatch)
                  (t :installed))
            :bundled bundled
            :expected-installed expected-installed
            :installed installed
            :bundled-version bundled-version
            :installed-version installed-version)))

(defun %quicklisp-directory-setf-example ()
  (format nil "(setf clhs:*quicklisp-directory* ~S)"
          (directory-namestring
           (make-pathname :directory '(:absolute "path" "to" "quicklisp")))))

(defun install-clhs-use-local (&key if-exists
                               (verbose t)
                               (destination-directory *quicklisp-directory*)
                               ensure-directories-exist-p)
  (if ensure-directories-exist-p
      (ensure-directories-exist destination-directory :verbose verbose))
  (if (and (not ensure-directories-exist-p)
           (not (%probe-file destination-directory)))
      (let ((directory (directory-namestring destination-directory)))
        (if (pathname-match-p destination-directory *quicklisp-directory*)
            (format t "The following doesn't seem to be ~
                       the correct quicklisp directory location:~@
                       \"~A\" does not exist.~@
                       Try ~A."
                    directory (%quicklisp-directory-setf-example))
            (format t "Destination directory \"~A\" does not exist."
                    directory)))
      (%copy-file (%clhs-use-local *system-directory*)
                  (%clhs-use-local destination-directory)
                  :if-destination-exists if-exists)))

(defun emacs-setup-form (&key (root (hyperspec-root))
                         (indirect-through-quicklisp-p t)
                         (quicklisp-directory *quicklisp-directory*))
  (if indirect-through-quicklisp-p
      (format nil "(load ~S t)"
              (namestring (%clhs-use-local quicklisp-directory)))
      (format nil "(setq common-lisp-hyperspec-root~%      \"file://~A\")"
              (namestring root))))

(defgeneric %print (kind &key)
  (:method ((kind (eql :not-installed)) &key)
    (format t "~
\~2&clhs-use-local.el was not found in your quicklisp directory.
This means you're at step 1 of 2 for configuring Emacs/Slime
to perform lookups/browsing with your local copy of the CLHS.

Please run (clhs:install-clhs-use-local) in the (Common Lisp) REPL.
This will install clhs-use-local.el in your quicklisp directory.

Then, run (clhs:print-emacs-setup-form) again for instructions for step 2.~2%"))
  (:method ((kind (eql :version-mismatch))
            &key installed bundled installed-version bundled-version)
    (check-type installed-version string)
    (check-type bundled-version string)
    (format t "~
\~2&clhs-use-local.el was found in your quicklisp directory, as expected.

However, there's a version mismatch between it and
the one bundled by this version of the CLHS ASDF wrapper:

Installed version: ~A
  Bundled version: ~A

Please run (clhs:install-clhs-use-local :if-exists :supersede).
This will supersede your installed version with the bundled one, as follows:

~A (version ~A)
-- will be copied to and overwrite -->
~A (version ~A)~2%"
            installed-version
            bundled-version
            bundled
            bundled-version
            installed
            installed-version))
  (:method ((kind (eql :installed)) &rest keys)
    (format t "~
~2&(clhs-use-local.el was found in your quicklisp directory.
Moreover, its version matches the one bundled with this CLHS ASDF wrapper.
You may proceed with step 2 of 2 below.)~2%")
    (apply #'%print :step-2 keys))
  (:method ((kind (eql :step-2)) &key emacs-setup-form readme-location)
    (format t "~
~2&Make Emacs evaluate this form to browse the CLHS locally:

~A


Use C-c C-d h make-instance RET to test if the change was successful.
If it was, then this will open your browser and the URL will begin with \"file:///\".

Put the form in your ~~/.emacs to persist the change for future sessions.


The README file has some further information,
including a list of 3 useful Slime CLHS lookup commands
and how to get Emacs to open CLHS pages in a different browser.
\(Location: ~A)~2%"
            emacs-setup-form
            readme-location)))

(defun print-emacs-setup-form (&key (root (hyperspec-root))
                               ((:indirect-through-quicklisp-p indirectp) t)
                               (quicklisp-directory *quicklisp-directory*))
  (let ((common
         (list :emacs-setup-form
               (emacs-setup-form :root root
                                 :indirect-through-quicklisp-p indirectp
                                 :quicklisp-directory quicklisp-directory)
               :readme-location
               (make-pathname :name "README"
                              :defaults *system-directory*))))
    (let ((directory (directory-namestring quicklisp-directory)))
      (format t "~2&[ Quicklisp directory: \"~A\" (~A)~%  ~
                      If the above location is not correct, do:~%  ~A ]"
              directory
              (if (%probe-file directory) "exists" "DOES NOT exist")
              (%quicklisp-directory-setf-example)))
    (if (not indirectp)
        (apply #'%print :step-2 common)
        (multiple-value-call #'%print
          (clhs-use-local-status :quicklisp-directory quicklisp-directory)
          :allow-other-keys t
          (values-list common))))
  (values))

(defun %maybe-print-clhs-use-local-status-warning
    (&key (stream *query-io*) (quicklisp-directory *quicklisp-directory*)
     ;; As of 18 april 2012 it seems Swank erroneously has
     ;; #'interactive-stream-p always return NIL for its streams.
     (interactive-test-p (unless (member :swank *features*)
                           t)))
  (let ((runprompt
         "Please run (clhs:print-emacs-setup-form) for details")
        (setf-details
         (format nil "(That command will also tell you how to set~@
                       another quicklisp directory location, if necessary.)"))
        (status (clhs-use-local-status
                 :quicklisp-directory quicklisp-directory)))
    (when (or (not interactive-test-p)
              (interactive-stream-p stream))
      (case status
        (:not-installed (format stream "~
~2&clhs-use-local.el doesn't seem to have been installed.
\(Assuming ~S is the correct quicklisp directory location.)~2%~A~@
on how to setup Emacs/Slime to perform lookups/browsing
with your local copy of the CLHS provided by this wrapper.~2%~A~2%"
                                (directory-namestring quicklisp-directory)
                                runprompt
                                setf-details))
        (:version-mismatch (format stream "~
~2&clhs-use-local.el version mismatch detected.~2%~A.~2%~A~2%"
                                   runprompt
                                   setf-details))))
    status))

(%maybe-print-clhs-use-local-status-warning)
