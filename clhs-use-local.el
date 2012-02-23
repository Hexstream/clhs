;;;; Version: 0.3
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
      (let ((relative (quicklisp-slime-helper-file-contents location-file)))
        (file-name-directory (concat quicklisp-clhs-base relative))))))

(setq common-lisp-hyperspec-root
      (concat "file:"
              (quicklisp-clhs-system-location)
              "HyperSpec-7-0/HyperSpec/"))
