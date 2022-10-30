;; -*- mode: lisp -*-

(defsystem "tictactoe"
  :version "0.0.1"
  :author "Hugo Sansaqua"
  :license "MIT"
  :depends-on ("cp/integer-pack")
  :components ((:module "src"
                :components
                ((:file "tictactoe"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "tictactoe/test"))))

(defsystem "tictactoe/test"
  :depends-on ("tictactoe"
               "fiveam")
  :components ((:file "test"))
  :description "Test system for tictactoe"
  :perform (test-op (o c)
                    (uiop:eval-input "(fiveam:run! 'tictactoe/test:main-suite)")))
