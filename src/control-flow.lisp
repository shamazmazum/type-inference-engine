(in-package :type-inference-engine)

(sera:defconstructor statement
  (assigns-to symbol)
  (function   symbol)
  (arguments  list)) ; of variables (symbols)

(defmethod print-object ((statement statement) stream)
  (print-unreadable-object (statement stream :type t)
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

(defun expand-literals (expr literal-initializers)
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

(defun expand-atom (expr)
  "Expand atomic expressions to a trivial function call."
  (if (atom expr) (list 'identity expr) expr))

;; KLUDGE: I just substitude bindings in LET as if it was
;; SYMBOL-MACROLET rather than a variable.
(defun expand-let (expr)
  "Substitute variables in the body of LET form with corresponding
bindings."
  (unless (and (eq (first expr) 'let)
               (= (length expr) 3))
    (error 'parser-error :code expr))
  (let ((bindings
         (loop for binding in (second expr) collect
               (if (and (= (length binding) 2)
                        (symbolp (first binding)))
                   (cons (first binding) (second binding))
                   (error 'parser-error :code expr)))))
    (labels ((%go (expr)
               (cond
                 ((member expr bindings :key #'car :test #'eq)
                  (alex:assoc-value bindings expr))
                 ((atom expr) expr)
                 (t
                  (cons (car expr)
                        (mapcar #'%go (cdr expr)))))))
      (%go (third expr)))))

(defun expand-lets (expr)
  "Expand all LET forms in EXPR"
  (labels ((%%go (expr)
             (cond
               ((atom expr) expr)
               ((eq (first expr) 'let)
                (expand-let expr))
               (t
                (cons (car expr)
                      (mapcar #'%%go (cdr expr))))))
           (%go (expr)
             (let ((expanded (%%go expr)))
               (if (equalp expr expanded) expr
                   (%go expanded)))))
    (%go expr)))

(defun expand-expression (expr literal-initializers)
  "Replace literals in the expression by corresponding initializers,
replace atomic expressions with a call to IDENTITY, etc."
  (expand-lets
   (expand-literals
    (expand-atom expr)
    literal-initializers)))

(defun collect-free-variables (expr)
  "Return a list of free variables in an expression"
  (labels ((%go (expr)
             (if (symbolp expr) (list expr)
                 (reduce #'append (cdr expr)
                         :initial-value nil
                         :key #'%go))))
    (remove-duplicates
     (%go expr)
     :test #'eq)))

(defun assign-depth (expr &optional parameter-vars)
  "Convert an S-expression into a list of pairs @c((depth . funcall))
where greater depth means earlier execution. The list
@c(parameter-vars) can optionally contain known variables, so this
function will signal @c(unknown-variable) condition if it encounters a
variable not present in this list."
  (labels ((%go (expr level)
             (if (symbolp expr)
                 (and parameter-vars
                      (not (member expr parameter-vars :test #'eq))
                      (error 'unknown-variable :variable expr))
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

;; FIXME: Assoc with default anywhere?
(defun assoc-default (item alist default)
  (let ((value (assoc item alist)))
    (if value (cdr value) default)))

(defun add-sf-node (nodes variables sf-label initializers)
  "Add start/finish node on top of the nodes. The start/finish node has a label
@c(sf-label) and initializes variables @c(variables)."
  (let ((first-label (ir-node-label (car nodes))))
    (cons
     (ir-node sf-label (list first-label)
              (mapcar
               (lambda (variable)
                 (statement variable
                            (assoc-default variable initializers 'initialize)
                            nil))
               variables))
     nodes)))

(defun parse-expr-to-ir (expr &optional literal-initializers)
  "Parse an expression (nested function calls) to intermediate representation"
  (let* ((expanded-expr (expand-expression expr literal-initializers))
         (expr-groups (group-by-depth (assign-depth expanded-expr)))
         (variable-mappings (allocate-variables expr-groups))
         (statement-groups (convert-to-statements expr-groups variable-mappings))
         (sf-label (gensym "S/F"))
         (ir-nodes (ir-nodes statement-groups sf-label)))
    (add-sf-node ir-nodes
                 (append (collect-free-variables expanded-expr)
                         (mapcar #'cdr variable-mappings))
                 sf-label nil)))

(sera:-> initializers (hash-table type-node symbol list)
         (values list &optional))
(defun initializers (fndb top name parameters)
  (let ((entry (gethash name fndb)))
    (when (typep entry 'simple-known-function)
      (let ((actual-arity (length parameters))
            (expected-arity (known-function-arity entry)))
        (unless (= actual-arity expected-arity)
          (error 'arity-error
                 :name     (known-function-name entry)
                 :actual   actual-arity
                 :expected expected-arity)))
      (remove
       nil (mapcar
            (lambda (parameter type)
              (let ((initializer (find-initializer fndb top type)))
                (if initializer (cons parameter initializer))))
            parameters (simple-known-function-arg-types entry))))))

(defun parse-defun-to-ir (db top form &optional literal-initializers)
  "Parse definition of a new function to intermediate representation"
  (unless (= (length form) 4)
    (error 'parser-error :code form))
  (destructuring-bind (defun-symb name parameters expr)
      form
    (unless (eq defun-symb 'defun)
      (error 'parser-error :code form))
    (let* ((initializers (initializers db top name parameters))
           (expanded-expr (expand-expression expr literal-initializers))
           (expr-groups (group-by-depth (assign-depth expanded-expr parameters)))
           (variable-mappings (allocate-variables expr-groups))
           (statement-groups (convert-to-statements expr-groups variable-mappings))
           (sf-label (gensym "S/F"))
           (ir-nodes (ir-nodes statement-groups sf-label)))
      (values
       (add-sf-node ir-nodes (append parameters (mapcar #'cdr variable-mappings))
                    sf-label initializers)
       name parameters))))

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

(sera:-> parse-defun (hash-table type-node list &optional list)
         (values list symbol list &optional))
(defun parse-defun (db top form &optional literal-initializers)
  "Parse definition of a new function to a model acceptable by INFER-TYPES."
  (multiple-value-bind (ir-nodes name parameters)
      (parse-defun-to-ir db top form literal-initializers)
    (values (ir-nodes->flat-nodes ir-nodes)
            name parameters)))

(sera:-> parse-expr (t &optional list)
         (values list &optional))
(defun parse-expr (code &optional literal-initializers)
  "Parse an expression @c(code) and construct a control flow
graph. @c(Literal-initializers) is an associative list which contains
predicates as keys and initializer functions as values. When
@c(parse-code) sees a term of the expression, it runs predicates in
@c(literal-initializers) (in the order of appearance) on that term. If
a predicate returns @c(T) then that term is considered as a literal
and the corresponding initializer function is used to initialize a
constant variable with a value of type of the literal. For example:
@begin[lang=lisp](code)
CL-USER> (parse-code '(+ x 3)) ; => Signals UNKNOWN-LITERAL
CL-USER> (type-inference-engine:parse-expr
          '(+ x 3)
          (list
           (cons #'integerp 'init/integer)))
((TYPE-INFERENCE-ENGINE:FLAT-CONTROL-NODE (2) (1) (#<TYPE-INFERENCE-ENGINE:STATEMENT X ← (INITIALIZE )>
                                                   #<TYPE-INFERENCE-ENGINE:STATEMENT RES567 ← (INITIALIZE )>
                                                   #<TYPE-INFERENCE-ENGINE:STATEMENT VAR568 ← (INITIALIZE )>))
 (TYPE-INFERENCE-ENGINE:FLAT-CONTROL-NODE (0) (2) (#<TYPE-INFERENCE-ENGINE:STATEMENT VAR568 ← (INIT/INTEGER )>))
 (TYPE-INFERENCE-ENGINE:FLAT-CONTROL-NODE (1) (0) (#<TYPE-INFERENCE-ENGINE:STATEMENT RES567 ← (+ X
                                                                                                 #:VAR568)>)))
@end(code)

This function returns a control flow graph which can be passed to @c(infer-types)."
  (let ((ir-nodes (parse-expr-to-ir code literal-initializers)))
    (ir-nodes->flat-nodes ir-nodes)))

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

(sera:-> result-variable (list)
         (values symbol &optional))
(defun result-variable (flat-nodes)
  "Get the variable in which the result of execution of a program is
stored."
  (let ((last-statement-group (flat-control-node-statements
                               (car (last flat-nodes)))))
    (assert (= (length last-statement-group) 1))
    (statement-assigns-to (first last-statement-group))))
