(in-package :type-inference-engine)

(define-condition inference-error (error)
  ()
  (:documentation "General type inference error"))

(define-condition functional-condition ()
  ((name :reader  unknown-function-name
         :type    symbol
         :initarg :name)))

(define-condition cycle-detected (inference-error)
  ((node :reader  cycle-detected-node
         :initarg :node))
  (:report
   (lambda (c s)
     (format s "Cycle detected in graph ~a"
             (cycle-detected-node c)))))

(define-condition typesystem-error (inference-error)
  ((type-system :reader  typesystem-error-system
                :initarg :type-system))
  (:report
   (lambda (c s)
     (format s "Type system ~a is defined incorrectly"
             (typesystem-error-system c)))))

(define-condition unknown-literal (inference-error)
  ((code :reader  unknown-literal-code
         :initarg :code))
  (:report
   (lambda (c s)
     (format s "Don't know what ~a means"
             (unknown-literal-code c)))))

(define-condition incorrect-definition (inference-error functional-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Type inference functions for function ~a are defined incorrectly"
             (unknown-function-name c)))))

(define-condition unknown-function (inference-error functional-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Unknown function with name ~a"
             (unknown-function-name c)))))

(define-condition arity-error (inference-error functional-condition)
  ((actual-arity   :reader  actual-arity
                   :type    alex:non-negative-fixnum
                   :initarg :actual)
   (expected-arity :reader  expected-arity
                   :type    alex:non-negative-fixnum
                   :initarg :expected))
  (:report
   (lambda (c s)
     (format s "Arity mismatch for function ~a: Actual ~d, expected ~d"
             (unknown-function-name c)
             (actual-arity c)
             (expected-arity c)))))

(define-condition fndb-entry-exists (inference-error functional-condition)
  ()
  (:report
   (lambda (c s)
     (format s "A function with name ~a is already in the database"
             (unknown-function-name c)))))
