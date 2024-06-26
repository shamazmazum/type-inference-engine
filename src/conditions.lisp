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

(define-condition parser-error (inference-error)
  ((code :reader        parser-error-code
         :initarg       :code
         :documentation "Parser error"))
  (:report
   (lambda (c s)
     (format s "Parser error: ~a"
             (parser-error-code c))))
  (:documentation "Signaled when parsing malformed code."))

(define-condition typecheck-error (inference-error functional-condition)
  ((varname  :reader        typecheck-error-varname
             :initarg       :varname
             :documentation "Name of the variable which holds a value of invalid type")
   (actual   :reader        typecheck-error-actual
             :initarg       :actual
             :documentation "Actual type of a value stored in the variable")
   (expected :reader        typecheck-error-expected
             :initform      nil
             :initarg       :expected
             :documentation "Expected type of a value stored in the variable"))
  (:report
   (lambda (c s)
     (format s
             "Type check error for function ~a, variable ~a. Actual: ~a~:[~;, expected: ~a~]"
             (function-name c)
             (typecheck-error-varname c)
             (typecheck-error-actual c)
             (typecheck-error-expected c)
             (typecheck-error-expected c))))
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

(define-condition cannot-redefine (inference-error functional-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Cannot redefine a primitive function ~a"
             (function-name c))))
  (:documentation "Signaled when an attempt to redefine a primitive
function is made."))
