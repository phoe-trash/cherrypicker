;;;; cherrypicker.lisp

(defpackage #:cherrypicker
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum)
                    (#:ss #:split-sequence)
                    (#:p #:plump))
  (:export #:frob))

(in-package #:cherrypicker)

;;; Whitespace stripper

(defvar *whitespace* '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page))

(defun strip-whitespace-only (node)
  (p:traverse
   node
   #'(lambda (node)
       (when (every (a:rcurry #'member *whitespace*) (p:text node))
         (p:remove-child node)))
   :test #'p:text-node-p))

(defun load-book (pathname)
  (strip-whitespace-only (p:parse pathname)))

;;; Walking stream

(defvar *walking-stream* *standard-output*)

(defun out (&rest args)
  (apply #'format *walking-stream* args))

;;; API

(defparameter *prologue*
  (asdf:system-relative-pathname :cherrypicker "prologue.html"))

(defparameter *epilogue*
  (asdf:system-relative-pathname :cherrypicker "epilogue.html"))

(defun frob (pathname &optional stream)
  (let ((*walking-stream* (or stream *walking-stream*)))
    (out (a:read-file-into-string *prologue*))
    (walk (load-book pathname))
    (out (a:read-file-into-string *epilogue*))))

;;; Walker

(defun walk (element)
  (flet ((tag-name= (x) (and (p:element-p element)
                             (string= (p:tag-name element) x))))
    (cond
      ((p:xml-header-p element) nil)
      ((p:root-p element) (walk-element element))
      ((tag-name= "node") (walk-node element))
      ((tag-name= "rich_text") (walk-rich-text element))
      ((p:element-p element) (walk-element element)))))

(defun walk-element (element)
  (map nil #'walk (p:children element)))

;;; Walk node

(defvar *heading-level* 0)

(defun rich-text-node-p (element)
  (and (p:element-p element) (string= (p:tag-name element) "rich_text")))

(defun walk-node (element)
  (let* ((*heading-level* (1+ *heading-level*))
         (title (p:attribute element "name")))
    (out "<div>~%")
    (out "<h~D>~A</h~D>~%" *heading-level* title *heading-level*)
    (multiple-value-bind (text nontext)
        (s:partition #'rich-text-node-p (p:children element))
      (unless (a:emptyp text)
        (out "<p>")
        (map nil #'walk text)
        (out "</p>~%"))
      (map nil #'walk nontext))
    (out "</div>~%")))

;;; Walk rich text

(defun walk-rich-text (element)
  (let* ((b (string= "heavy" (p:attribute element "weight")))
         (i (string= "italic" (p:attribute element "style")))
         (u (string= "single" (p:attribute element "underline")))
         (text (p:encode-entities (p:text element))))
    (loop for (string . next) on (ss:split-sequence #\Newline text)
          do (out "~:[~;<b>~]~:[~;<i>~]~:[~;<u>~]" b i u)
             (out "~A" string)
             (out "~:[~;</u>~]~:[~;</i>~]~:[~;</b>~]" u i b)
          if (consp next)
            do (out "</p>~%<p>")
          else do (loop-finish))))
