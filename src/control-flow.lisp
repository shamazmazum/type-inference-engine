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

(defun check-statements (statements)
  (unless (= (length statements)
             (length (remove-duplicates statements :key #'statement-assigns-to)))
    ;; Internal check, so I do not use any distinct condition class
    (error "Wrong statement: duplicate variables on the left-hand side: ~a"
           statements)))

(sera:defconstructor ir-node
  (label      symbol)
  (goes-to    list)
  (statements list))

(sera:-> literal-initializer (t list)
         (values (or null symbol) &optional))
(defun literal-initializer (code table)
  "If CODE is a literal return a function which initializes a variable
with the type of that literal or NIL otherwise."
  (let ((entry (find-if (alex:rcurry #'funcall code) table :key #'car)))
    (cdr entry)))

(defun handle-literals (expr literal-initializers)
  "Return @c(expr) with literals replaced by corresponding literal
initializer functions."
  (labels ((%go (expr)
             (let ((initializer (literal-initializer expr literal-initializers)))
               (cond
                 (initializer
                  ;; Replace a literal with a call to a special function
                  ;; ("literal initializer") which returns a value of needed
                  ;; type.
                  (list initializer))
                 ((symbolp expr)
                  ;; This is a variable
                  expr)
                 ((atom expr)
                  (error 'unknown-literal :code expr))
                 (t
                  (cons (car expr)
                        (mapcar #'%go (cdr expr))))))))
    (%go expr)))

(defun collect-free-variables (expr)
  "Return a list of free variables in an expression"
  (labels ((%go (expr)
             (if (symbolp expr) (list expr)
                 (reduce #'append (cdr expr)
                         :initial-value nil
                         :key #'%go))))
    (%go expr)))

(defun assign-depth (expr)
  "Convert an S-expression into a list of pairs @c((depth . funcall))
where greater depth means earlier execution."
  (labels ((%go (expr level)
             (if (not (symbolp expr))
                 (reduce #'append (cdr expr)
                         :initial-value (list (cons level expr))
                         :key (lambda (expr)
                                (%go expr (1+ level)))))))
    (%go expr 0)))

(defun group-by-depth (subexprs)
  "Group funcalls by precedence of execution"
  (let ((groups (sera:assort subexprs :test #'= :key  #'car)))
    (mapcar (alex:curry #'mapcar #'cdr) groups)))

(defun allocate-variables (groups)
  "Allocate intermediate variables for groups of funcalls"
  (assert (= (length (first groups)) 1))
  (let ((res-var (gensym "RES")))
    (cons
     (cons (caar groups) res-var)
     (reduce #'append (cdr groups)
             :key (lambda (group)
                    (mapcar
                     (lambda (expr)
                       (cons expr (gensym "VAR")))
                     group))))))

(defun convert-to-statements (groups variable-mappings)
  "Convert groups of funcalls (which look more or less like an ordinary lisp
code) to a group of assignment statements. Each group of statements can be
considered as a parallel assignment statement."
  (mapcar
   (lambda (group)
     (mapcar
      (lambda (expr)
        (let ((variable (alex:assoc-value variable-mappings expr :test #'eq)))
          (statement
           variable   ; an intermediate variable to assign result to
           (car expr) ; name of a function to call
           (mapcar    ; argument variables
            (lambda (argument)
              (if (symbolp argument) argument
                  (alex:assoc-value variable-mappings argument :test #'eq)))
            (cdr expr)))))
      group))
   (reverse groups)))

(defun ir-nodes (groups sf-label)
  "Convert groups of statements to intermediate representation node (which has a
label and a list of labels to which it transfers the control). The last node
transfers control to a node labeled with @c(sf-label)."
  (labels ((collect-lbls (n acc)
           (if (zerop n) acc (collect-lbls (1- n) (cons (gensym "LBL") acc)))))
    (let ((lbls (collect-lbls (length groups) (list sf-label))))
      (mapcar
       (lambda (group label next-label)
         (ir-node label (list next-label) group))
       groups lbls (cdr lbls)))))

(defun add-sf-node (nodes variables sf-label)
  "Add start/finish node on top of the nodes. The start/finish node has a label
@c(sf-label) and initializes variables @c(variables)."
  (let ((first-label (ir-node-label (car nodes))))
    (cons
     (ir-node sf-label (list first-label)
              (mapcar
               (lambda (variable)
                 (statement variable 'initialize nil))
               variables))
     nodes)))

(defun parse-expr (expr &optional literal-initializers)
  "Parse an expression (nested function calls) to intermediate representation"
  (let* ((no-literals (handle-literals (if (atom expr) (list 'identity expr) expr)
                                       literal-initializers))
         (expr-groups (group-by-depth (assign-depth no-literals)))
         (variable-mappings (allocate-variables expr-groups))
         (statement-groups (convert-to-statements expr-groups variable-mappings))
         (sf-label (gensym "S/F"))
         (ir-nodes (ir-nodes statement-groups sf-label)))
    (add-sf-node ir-nodes
                 (append (remove-duplicates (collect-free-variables no-literals)
                                            :test #'eq)
                         (mapcar #'cdr variable-mappings))
                 sf-label)))

;; A structure to represent a control flow graph as a flat list
(sera:defconstructor flat-control-node
  (from       list)
  (to         list)
  (statements list))

(defun who-goes-to (nodes node)
  "Get nodes which transfer control to @c(node)."
  (let ((label (ir-node-label node)))
    (remove-if
     (lambda (node)
       (not (member label (ir-node-goes-to node))))
     nodes)))

(defun goes-to (nodes node)
  "Get nodes to which @c(node) transfers control."
  (let ((goes-to (ir-node-goes-to node)))
    (mapcar
     (lambda (label)
       (find label nodes :key #'ir-node-label))
     goes-to)))

(defun ir-nodes->flat-nodes (ir-nodes)
  "Resolve labels in the intermediate representation"
  (mapcar
   (lambda (node)
     (flat-control-node
      (mapcar
       (lambda (node) (position node ir-nodes))
       (who-goes-to ir-nodes node))
      (mapcar
       (lambda (node) (position node ir-nodes))
       (goes-to ir-nodes node))
      (ir-node-statements node)))
   ir-nodes))

;; TODO: Add support for special forms like if and goto
(sera:-> parse-code (t &optional list)
         (values list symbol &optional))
(defun parse-code (code &optional literal-initializers)
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
  (let ((ir-nodes (parse-expr code literal-initializers)))
    (values
     (ir-nodes->flat-nodes ir-nodes)
     (let ((statements (ir-node-statements (car (last ir-nodes)))))
       (assert (= (length statements) 1))
       (statement-assigns-to (first statements))))))

(sera:-> program-variables (list)
         (values list &optional))
(defun program-variables (flat-graph)
  "Collect all variables occuring in the program."
  (remove-duplicates
   (reduce #'append flat-graph
           :key (lambda (node)
                  (reduce #'append (flat-control-node-statements node)
                          :key (lambda (statement)
                                 (cons (statement-assigns-to statement)
                                       (statement-arguments  statement)))
                          :initial-value nil))
           :initial-value nil)
   :test #'eq))
