#|
  This file is a part of cl-qiita project.
|#

(defsystem "cl-qiita-test"
    :defsystem-depends-on ("prove-asdf")
    :author "takeokunn"
    :license "GPLv3"
    :depends-on ("cl-qiita"
                    "prove")
    :components ((:module "tests"
                     :components
                     ((:test-file "cl-qiita"))))
    :description "Test system for cl-qiita"

    :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
