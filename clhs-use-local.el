;;;; Version: 0.6
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

(defun quicklisp-clhs-setup-symlink (&optional relativep)
  (let ((absolute (quicklisp-clhs-hyperspec-location)))
    (make-symbolic-link (if relativep
                            (file-relative-name absolute
                                                quicklisp-clhs-base)
                          absolute)
                        (quicklisp-clhs-symlink-location)
                        t))
  ;; Ideally we'd detect if symlink creation was actually successful.
  ;; Let's just assume it was, for now.
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
