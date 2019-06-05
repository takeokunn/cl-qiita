(defpackage cl-qiita.base
    (:use :cl)
    (:export :http-get))
(in-package :cl-qiita)

(defvar *URL* "https://qiita.com")

(defmacro generate-url (path &optional query)
    `(quri:make-uri
         :defaults *URL*
         :path ,path
         :query ,query))

(defmacro base-client (&body body)
    `(multiple-value-bind (response status) ,@body
         (values response status)))

(defun http-get (&key path)
    (base-client (dex:get (generate-url path)
                     :headers '(("Content-Type" . "application/json")))))

;; (defun http-post (&key path content (token nil))
;;     (base-client (dex:post (generate-url path)q
;;                      :content content)))
