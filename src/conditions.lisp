(in-package :type-inference-engine)

(define-condition inference-error (error)
  ()
  (:documentation "General type inference error"))

(define-condition functional-condition ()
  ((name :reader        function-name
         :type          symbol
         :initarg       :name
         :documentation "Name of a function which has caused an error")))

(define-condition cycle-detected (inference-error)
  ((node :reader        cycle-detected-node
         :initarg       :node
         :documentation "An already seen node"))
  (:report
   (lambda (c s)
     (format s "Cycle detected in graph ~a"
             (cycle-detected-node c))))
  (:documentation "Signaled when a cycle is detected during a graph
traversal and that graph cannot contain cycles."))

(define-condition typesystem-error (inference-error)
  ((type-system :reader        typesystem-error-system
                :initarg       :type-system
                :documentation "A top node of the type system"))
  (:report
   (lambda (c s)
     (format s "Type system ~a is defined incorrectly"
             (typesystem-error-system c))))
  (:documentation "Signaled when a type system is defined
incorrectly."))

(define-condition unknown-literal (inference-error)
  ((code :reader        unknown-literal-code
         :initarg       :code
         :documentation "An expression which contains unknown literal"))
  (:report
   (lambda (c s)
     (format s "Don't know what ~a means"
             (unknown-literal-code c))))
  (:documentation "Signaled when S-expression looks like a literal but
its type cannot be determined."))

(define-condition unknown-variable (inference-error)
  ((variable :reader        unknown-variable
             :initarg       :variable
             :documentation "Name of unknown variable"))
  (:report
   (lambda (c s)
     (format s "Unknown variable encountered: ~a"
             (unknown-variable c))))
  (:documentation "Signaled when an unknown variable is encountered
when parsing @c(defun)."))

(define-condition malformed-defun (inference-error)
  ((code :reader        malformed-defun-code
         :initarg       :code
         :documentation "Malformed defun form"))
  (:report
   (lambda (c s)
     (format s "Malformed defun form: ~a"
             (malformed-defun-code c))))
  (:documentation "Signaled when parsing malformed @c(defun)."))

(define-condition typecheck-error (inference-error functional-condition)
  ((argtypes :reader        typecheck-error-argtypes
             :initarg       :argtypes
             :documentation "Types of arguments")
   (restype  :reader        typecheck-error-restype
             :initarg       :restype
             :documentation "Type of result"))
  (:report
   (lambda (c s)
     (format s "Type check error for function ~a: ~a → ~a"
             (function-name c)
             (typecheck-error-argtypes c)
             (typecheck-error-restype c))))
  (:documentation "Signaled when a definition of a function contains
type errors"))

(define-condition incorrect-definition (inference-error functional-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Type inference functions for function ~a are defined incorrectly"
             (function-name c))))
  (:documentation "Signaled when type inference function is defined
incorrectly."))

(define-condition unknown-function (inference-error functional-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Unknown function with name ~a"
             (function-name c))))
  (:documentation "Signaled when an unknown function is called in the
code which needs to be type-checked."))

(define-condition arity-error (inference-error functional-condition)
  ((actual-arity   :reader        actual-arity
                   :type          alex:non-negative-fixnum
                   :initarg       :actual
                   :documentation "Arity of a function seen in the code")
   (expected-arity :reader        expected-arity
                   :type          alex:non-negative-fixnum
                   :initarg       :expected
                   :documentation "Arity of a function in FNDB"))
  (:report
   (lambda (c s)
     (format s "Arity mismatch for function ~a: Actual ~d, expected ~d"
             (function-name c)
             (actual-arity c)
             (expected-arity c))))
  (:documentation "Signaled when a known function is called with wrong
number of arguments."))

(define-condition fndb-entry-exists (inference-error functional-condition)
  ()
  (:report
   (lambda (c s)
     (format s "A function with name ~a is already in the database"
             (function-name c))))
  (:documentation "Signaled when you want to place an entry in fndb
which tries to overwrite another existing entry."))
