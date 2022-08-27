;;;; cherrypicker.asd

(asdf:defsystem #:cherrypicker
  :description "Utility for exporting CherryTree XML documents to better HTML."
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:serapeum
               #:split-sequence
               #:plump)
  :components ((:file "cherrypicker")))
