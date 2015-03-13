(asdf:defsystem #:clhs

  ;; For more detailed CLHS authorship information, consult:
  ;; http://www.lispworks.com/documentation/HyperSpec/Front/Help.htm#Authorship
  :author "Common Lisp Specification: X3J13 Committee
HyperSpec: Kent Pitman <kmp@lispworks.com>
HyperSpec funder and copyright holder: LispWorks ltd. <support@lispworks.com>
Thin ASDF wrapper (excluding HyperSpec): Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the LICENSE file for details.
  :license "HyperSpec: Proprietary;
Thin ASDF wrapper (excluding HyperSpec): Public Domain"

  ;; See the README file for a longer description.
  :description "The HyperSpec-7-0 directory in this thin ASDF wrapper is a complete and unmodified copy of Lispworks' Common Lisp HyperSpec version 7.0 (referenced from <http://www.lispworks.com/documentation/common-lisp.html>). Redistribution of the HyperSpec is made with permission from LispWorks per the terms and restrictions set forth at <http://www.lispworks.com/documentation/HyperSpec/Front/Help.htm#Legal>. You may further redistribute the HyperSpec subject to the same terms and restrictions; consult the previous link for all details."

  :version "0.6.3"
  :serial cl:t
  :components ((:module "HyperSpec-7-0")
	       (:file "package")
	       (:file "main")))
