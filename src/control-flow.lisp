(in-package :type-inference-engine)

(sera:defconstructor statement
  (assigns-to symbol)
  (function   symbol)
  (arguments  list)) ; of variables (symbols)

(defmethod print-object ((statement statement) stream)
  (print-unreadable-object (statement stream :identity t :type t)
    (format stream "~a ← (~a "
            (statement-assigns-to statement)
            (statement-function   statement))
    (pprint-fill stream (statement-arguments statement) nil)
    (princ ")" stream)))

(sera:defconstructor control-flow-node
  (statements      list) ; of statements
  (direct-subnodes list))

(defmethod print-object ((node control-flow-node) stream)
  (print-unreadable-object (node stream :identity t)
    (format stream "Node ~@<~a~:>"
            (control-flow-node-statements node))))

(defun check-statements (statements)
  (unless (= (length statements)
             (length (remove-duplicates (mapcar #'statement-assigns-to statements))))
    ;; Internal check, so I do not use any distinct condition class
    (error "Wrong statement: duplicate variables on the left-hand side: ~a"
           statements)))

(sera:-> literal-initializer (t list)
         (values (or null symbol) &optional))
(defun literal-initializer (code table)
  "If CODE is a literal return a function which initializes a variable
with the type of that literal or NIL otherwise."
  (let ((entry (find-if (alex:rcurry #'funcall code) table :key #'car)))
    (cdr entry)))

(sera:-> literal-initializer-node (symbol)
         (values control-flow-node symbol &optional))
(defun literal-initializer-node (initializer-function)
  (let ((var-name (gensym "CONST")))
    (values
     (control-flow-node
      (list (statement var-name initializer-function nil))
      nil)
     var-name)))

(sera:-> identity-node (symbol)
         (values control-flow-node symbol &optional))
(defun identity-node (variable)
  (values
   (control-flow-node
    (list (statement variable 'identity (list variable)))
    nil)
   variable))

;; TODO: 1) Produce less nodes (our assignment statements are parallel)
;;       2) Add support for special forms like if and goto
(sera:-> parse-code (t &optional list boolean)
         (values (or control-flow-node null) symbol &optional))
(defun parse-code (code &optional literal-initializers recursive-p)
  "Parse an expression @c(code) and construct a control flow
graph. @c(Literal-initializers) is an associative list which contains
predicates as keys and initializer functions as values. When
@c(parse-code) sees a term of the expression, it runs predicates in
@c(literal-initializers) (in the order of appearance) on that term. If
a predicate returns @c(T) then that term is considered as a literal
and the corresponding initializer function is used to initialize a
constant variable with a value of type of the literal. For example:
@begin[lang=lisp](code)
(parse-code '(+ x 3)) ; => Signals UNKNOWN-LITERAL
(parse-code
 '(+ x 3)
 (list
  (cons #'intergerp 'init/integer)))
;; =>
;; #<Node (#<STATEMENT VAR254 ← (+ X CONST253) {1010A14543}>) {1010A14563}>
;; #:VAR254
@end(code)

This function returns a control flow graph and the name of a variable
where the result of expression is stored after evaluation of a
statement."
  (let ((literal-initializer (literal-initializer code literal-initializers)))
    (cond
      (literal-initializer
       ;; Initialize result-var with a value of the literal's type.
       (literal-initializer-node literal-initializer))
      ((symbolp code)
       (if recursive-p
           ;; This is an evaluated argument inside other statement,
           ;; create no new node.
           (values nil code)
           ;; This is just a variable. Transform this to VAR ← IDENTITY(VAR)
           (identity-node code)))
      ((atom code)
       ;; Seems like an unknwon literal
       (error 'unknown-literal :code code))
      (t
       ;; Otherwise it is a function call (no let, if or goto special
       ;; forms by now).
       (destructuring-bind (fnname . args) code
         (let* ((argvars-and-subnodes
                 (loop for arg in args collect
                       (multiple-value-call #'cons
                         (parse-code arg literal-initializers t))))
                (subnodes (mapcar #'car argvars-and-subnodes))
                (argvars  (mapcar #'cdr argvars-and-subnodes))
                (result-var (gensym "VAR")))
           (values
            (control-flow-node
             (list
              (statement result-var fnname argvars))
             (remove nil subnodes))
            result-var)))))))

(sera:-> flatten-control-flow (control-flow-node)
         (values list &optional))
(defun flatten-control-flow (top)
  "Return a content of the control flow graph as a flat list."
  (fold-graph
   top
   (lambda (acc node)
     (cons node acc))
   nil #'control-flow-node-direct-subnodes))

;; A structure to represent a control flow graph as a flat list
(sera:defconstructor flat-control-node
  (from       list)
  (to         list)
  (statements list))

(sera:-> program-variables (list)
         (values list &optional))
(defun program-variables (flat-graph)
  (remove-duplicates
   (alex:flatten
    (mapcar
     (lambda (flat-node)
       (mapcar
        (lambda (statement)
          (cons (statement-assigns-to statement)
                (statement-arguments  statement)))
        (flat-control-node-statements flat-node)))
     flat-graph))))

;; TODO: This implementation does not support branching yet.
(sera:-> flat-control-flow-graph (control-flow-node)
         (values list &optional))
(defun flat-control-flow-graph (node)
  "Convert control flow graph returned from @c(parse-code) to a flat
representation understandable by @c(infer-types)."
  (let* ((statements-list
          (mapcar #'control-flow-node-statements
                  (flatten-control-flow node)))
         (length (length statements-list))
         (flat-graph
          (si:collect
              (si:imap
               (lambda (statements i)
                 ;; 1+ means we'll add initialization node later
                 (flat-control-node (list (mod (1- i) (1+ length)))
                                    (list (mod (1+ i) (1+ length)))
                                    statements))
               (si:list->iterator statements-list)
               (si:count-from 1)))))
    ;; Add initialization node
    (cons
     (flat-control-node
      (list length) (list 1)
      (mapcar
       (lambda (variable)
         (statement variable 'initialize nil))
       (program-variables flat-graph)))
     flat-graph)))
