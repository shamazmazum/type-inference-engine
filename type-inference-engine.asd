(defsystem :type-inference-engine
    :name :type-inference-engine
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Type inference engine which implements an algorithm
described in the paper \"A General Scheme for the Automatic Inference
of Variable Types\" by Marc Kaplan and Jeffrey Ullman."
    :license "2-clause BSD"
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "conditions")
                 (:file "graph-traversal")
                 (:file "type-system")
                 (:file "functions")
                 (:file "control-flow")
                 (:file "inference-rules"))
    :depends-on (:alexandria
                 :serapeum
                 :stateless-iterators)
    :in-order-to ((test-op (load-op "type-inference-engine/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (uiop:symbol-call :type-inference-engine/tests '#:run-tests)))

(defsystem :type-inference-engine/example
    :name :type-inference-engine/example
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :serial t
    :pathname "example"
    :components ((:file "package")
                 (:file "example"))
    :depends-on (:type-inference-engine
                 :alexandria))

(defsystem :type-inference-engine/tests
    :name :type-inference-engine/tests
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :pathname "tests"
    :serial t
    :components ((:file "package")
                 (:file "tests"))
    :depends-on (:type-inference-engine
                 :type-inference-engine/example
                 :fiveam
                 :alexandria
                 :serapeum))
