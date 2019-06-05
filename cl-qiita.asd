#|
  This file is a part of cl-qiita project.
|#

(defsystem "cl-qiita"
    :version "0.1.0"
    :author "takeokunn"
    :license "GPLv3"
    :depends-on (:dexador :cl-json :quri)
    :components ((:module "src"
                     :components
                     ((:file "cl-qiita" :depends-on ("base"))
                         (:file "base"))))
    :description ""
    :long-description
    #.(read-file-string
          (subpathname *load-pathname* "README.markdown"))
    :in-order-to ((test-op (test-op "cl-qiita-test"))))
