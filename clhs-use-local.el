;;;; Version: 0.7
;;;;
;;;; This file was installed by clhs (trivial ASDF wrapper), like this:
;;;; (clhs:install-clhs-use-local)
;;;;
;;;; Load this file from your ~/.emacs to use local CLHS, like this:
;;;; (load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

(setq quicklisp-clhs-dist "quicklisp")

(setq quicklisp-clhs-base
      (if load-file-name
          (file-name-directory load-file-name)
        (expand-file-name "~/quicklisp/")))

(defun quicklisp-clhs-file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun quicklisp-clhs-location-file ()
  (concat quicklisp-clhs-base
          "dists/"
          quicklisp-clhs-dist
          "/installed/systems/clhs.txt"))

(defun quicklisp-clhs-system-location ()
  (let ((location-file (quicklisp-clhs-location-file)))
    (when (file-exists-p location-file)
      (let ((relative (quicklisp-clhs-file-contents location-file)))
        (file-name-directory (concat quicklisp-clhs-base relative))))))

(defun quicklisp-clhs-symlink-location (&optional as-directory-p)
  (concat quicklisp-clhs-base
          "HyperSpec"
          (if as-directory-p "/" "")))

(defun quicklisp-clhs-hyperspec-location (&optional through-symlink-p)
  (if through-symlink-p
      (quicklisp-clhs-symlink-location t)
    (concat (quicklisp-clhs-system-location)
            "HyperSpec-7-0/HyperSpec/")))

(defun quicklisp-clhs-inhibit-symlink-p ()
  (and (boundp 'quicklisp-clhs-inhibit-symlink-p)
       quicklisp-clhs-inhibit-symlink-p))

(defun quicklisp-clhs-inhibit-symlink-relative-p ()
  (and (boundp 'quicklisp-clhs-inhibit-symlink-relative-p)
       quicklisp-clhs-inhibit-symlink-relative-p))

(defun quicklisp-clhs-resolve-symlink-as-folder (link)
  (concat (file-symlink-p link) "/"))

(defun quicklisp-clhs-ensure-symbolic-link (symlink-location path)
  (let ((current-path (quicklisp-clhs-resolve-symlink-as-folder symlink-location)))
    (when (or (not current-path) (not (string-equal current-path path)))
      (make-symbolic-link path symlink-location t))))

(defun quicklisp-clhs-setup-symlink (&optional relativep)
  (let ((symlink-location (quicklisp-clhs-symlink-location))
        (desired-path (let ((absolute (quicklisp-clhs-hyperspec-location)))
                        (if relativep
                            (file-relative-name absolute quicklisp-clhs-base)
                          absolute))))
    (condition-case nil
        (quicklisp-clhs-ensure-symbolic-link
         symlink-location
         desired-path)
      (file-error
       (with-output-to-temp-buffer "clhs (thin ASDF wrapper)"
         (princ (format "Sorry, unable to create symlink named \"%s\" pointing to \"%s\".

Would you happen to be using Windows 10? Then you may need administrator privileges to be able to create symlinks. Please either run emacs as an administrator and then load clhs-use-local.el, or manually create the symlink with mklink in an administrator-mode console per the above parameters. This needs to be done only once, you should be able to use the CLHS wrapper as a normal user subsequently.

(TODO: Detect Windows 10 instead of unconditionally displaying the above note about Windows 10.)"
                        symlink-location
                        desired-path))))))
  t)

(defun quicklisp-clhs-setup-hyperspec-root (&optional through-symlink-p)
  (setq common-lisp-hyperspec-root
        (concat "file://"
                (quicklisp-clhs-hyperspec-location through-symlink-p))))

(quicklisp-clhs-setup-hyperspec-root
 (unless (quicklisp-clhs-inhibit-symlink-p)
   (quicklisp-clhs-setup-symlink
    (unless (quicklisp-clhs-inhibit-symlink-relative-p)
      t))))
