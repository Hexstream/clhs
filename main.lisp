(in-package #:clhs)

(defparameter *hyperspec-root*
  (merge-pathnames
   (make-pathname :directory '(:relative "HyperSpec-7-0" "HyperSpec")
		  :name nil :type nil)
   #.(or *compile-file-truename* *load-truename*)))

(defun %btw (message &rest args)
  (format *terminal-io* "~2&~?~%" message args))

(defun %section (title message &rest args)
  (format *terminal-io* "~2&--- ~? ---~2%~?~2%"
	  title nil message args))

(defparameter *emacs-setup-form*
  (format nil "(setq common-lisp-hyperspec-root~%      \"file:~A\")"
	  (directory-namestring *hyperspec-root*)))

(defvar *emacs-init-location*
  (merge-pathnames #P".emacs" (user-homedir-pathname)))

(defun print-emacs-setup-howto ()
  (%section "1. The Form"
	    *emacs-setup-form*)
  (%section "2. What is it for?"
	    "~
The above Emacs Lisp form (executable code) tells Emacs to use
your newly installed copy of the Common Lisp HyperSpec
instead of the online version at:
http://www.lispworks.com/documentation/HyperSpec/Front/index.htm")
  (%section "3. Why use your local copy of the CLHS as a reference?"
	    "~
To benefit from the increased responsiveness inherent in browsing a
local resource and to ensure reliable, fast access from anywhere while
on the go (ex: laptop, smartphone).")
  (%section "4. How to tell Emacs to use my local CLHS just for this session?"
	    "~
Put the point (cursor) after the closing parenthesis of The Form, then
invoke M-x eval-last-sexp. This tells Emacs to start using your local
CLHS immediately, however the change will not persist if you restart
Emacs (see next section for how to make the changes persist).

To test if the change was successful, see Section 6.")
  (%section "5. How to tell Emacs to always use my local CLHS in the future?"
	    "~

This is done by putting The Form in your .emacs file. Emacs executes
this file every time it starts, so your local CLHS will be used when
you next start Emacs (see previous section for how to apply the change
for the current session without restarting Emacs).

The expected location of your .emacs file on your system is:~%~S~%~A

Enter (clhs:install-emacs-setup-form) in the (Slime) REPL to
automatically append The Form at the end of your .emacs init file."
	    (namestring *emacs-init-location*)
	    (if (probe-file *emacs-init-location*)
		"(It indeed exists.)"
		"(It doesn't exist yet. It will be created as needed.)"))
  (%section "~
    6. How to test that the changes were successful ---
---    and everything works correctly?             "
	    "~
From a Slime buffer, invoke C-c C-d h (slime-documentation-lookup).
Then, write \"make-instance\" (without the quotes) and press Enter.
This should open your default browser at the CLHS page for function
MAKE-INSTANCE. The URL should look like this:

file://~A

If you see a lispworks.com URL instead,
then your local CLHS is not being used."
	    (directory-namestring *hyperspec-root*))
  (%section "7. What are the most useful Slime CLHS lookup commands?"
	    "~
slime-documentation-lookup (C-c C-d h)
Lookup a symbol (such as \"make-instance\").

common-lisp-hyperspec-lookup-reader-macro (C-c C-d #)
Lookup a reader-macro (such as \"#'\" or \"(\").

common-lisp-hyperspec-format (C-c C-d ~~)
Lookup a FORMAT directive (such as \"~~A\").

You can get a list of these and other related commands with C-c C-d C-h.")
  (%section "8. How to tell Emacs to open the CLHS with another browser?"
	    "~
Here's how you might tell Emacs to use Firefox:

\(setq browse-url-firefox-program \"firefox\")


For Google Chrome, try:

\(setq browse-url-browser-function 'browse-url-generic)
\(setq browse-url-generic-program \"google-chrome\")"))

(defun install-emacs-setup-form ()
  )
