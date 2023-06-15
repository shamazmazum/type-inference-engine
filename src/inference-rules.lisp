(in-package :type-inference-engine)

(deftype var->type   () '(cons symbol type-node))
(deftype vars->types () '(simple-array var->type (*)))
;; Program-wide mapping
(deftype wide-vars->types () '(simple-array vars->types (*)))
;; Inference operator
(deftype wide-inference-rule () '(simple-array (or null function) (* *)))

(sera:-> variables->types-forward
         (statement vars->types)
         (values list &optional))
(defun variables->types-forward (statement mappings)
  "Return types of variables on the right-hand side of a statement for
forward inference."
  (mapcar
   (lambda (variable)
     (let ((mapping (find variable mappings :key #'car)))
       (cdr mapping)))
   (statement-arguments statement)))

(sera:-> forward-inference-rule (list)
         (values (sera:-> (hash-table type-node vars->types)
                          (values vars->types &optional))
                 &optional))
(defun forward-inference-rule (statements)
  "Return a function of arguments FNDB, TOP type and MAPPINGS
(variable names set -> types set mapping) which returns a new variable
names set -> types set mapping after execution of STATEMENTS using
forward inference."
  (check-statements statements)
  (lambda (db top mappings)
    (map '(vector var->type)
         (lambda (mapping)
           (destructuring-bind (variable . type) mapping
             (cons variable
                   ;; Find any statement where this variable gets a
                   ;; new value (i.e. is placed on the left-hand
                   ;; side of a statement).
                   (let ((left-hand-statement (find variable statements
                                                    :key #'statement-assigns-to))
                         (right-hand-statements (remove-if-not
                                                 (alex:curry #'find variable) statements
                                                 :key #'statement-arguments)))
                     (cond
                       (left-hand-statement
                        ;; The variable appears on the left-hand in one of the node's statements
                        (apply #'apply-restype-fn db top
                               (statement-function left-hand-statement)
                               (variables->types-forward left-hand-statement mappings)))
                       (right-hand-statements
                        ;; The variable is on the right-hand of one or more statements
                        (reduce
                         (alex:curry #'meet top)
                         (map '(vector type-node)
                              (lambda (statement)
                                (let ((right-hand-positions
                                       (loop for var in (statement-arguments statement)
                                             for pos from 0 by 1
                                             when (eq var variable) collect pos))
                                      (right-hand-types (variables->types-forward
                                                         statement mappings)))
                                  (assert right-hand-positions)
                                  (reduce
                                   (alex:curry #'meet top)
                                   (map '(vector type-node)
                                        (lambda (position)
                                          (apply #'apply-argtype-fn db top
                                                 (statement-function statement)
                                                 position top
                                                 right-hand-types))
                                        right-hand-positions))))
                              right-hand-statements)))
                       (t
                        ;; Variable is not present anywhere is the node's statements
                        type))))))
         mappings)))

;; FIXME: Refactor this
(sera:-> variables->types-backward
         (list statement type-node vars->types)
         (values type-node list &optional))
(defun variables->types-backward (statements statement top mappings)
  "Return variables type on the left-hand and right-hand sides of a
statement for backward inference."
  (flet ((var-type (variable)
           (let ((mapping (find variable mappings :key #'car)))
             (cdr mapping))))
    (values
     (var-type (statement-assigns-to statement))
     (mapcar
      (lambda (variable)
        (if (find variable statements :key #'statement-assigns-to)
            top
            (var-type variable)))
      (statement-arguments statement)))))

(sera:-> backward-inference-rule (list)
         (values (sera:-> (hash-table type-node vars->types)
                          (values vars->types &optional))
                 &optional))
(defun backward-inference-rule (statements)
  "Return a function of arguments FNDB, TOP type and MAPPINGS
(variable names set -> types set mapping) which returns a new variable
names set -> types set mapping after execution of STATEMENTS using
backward inference."
  (check-statements statements)
  (lambda (db top mappings)
    (map '(vector var->type)
         (lambda (mapping)
           (destructuring-bind (variable . type) mapping
             (cons variable
                   ;; Find any statement where this variable gets a
                   ;; new value (i.e. is placed on the left-hand
                   ;; side of a statement).
                   (let ((left-hand-statement (find variable statements
                                                    :key #'statement-assigns-to))
                         (right-hand-statements (remove-if-not
                                                 (alex:curry #'find variable) statements
                                                 :key #'statement-arguments)))
                     (cond
                       ((not (or left-hand-statement right-hand-statements))
                        ;; Variable is not present anywhere is the node's statements
                        type)
                       ((and left-hand-statement (not right-hand-statements))
                        ;; Variable appears only on the left-hand side
                        top)
                       (t
                        ;; Variable appears on the right-hand side
                        (reduce
                         (alex:curry #'meet top)
                         (map '(vector type-node)
                              (lambda (statement)
                                (let ((right-hand-positions
                                       (loop for var in (statement-arguments statement)
                                             for pos from 0 by 1
                                             when (eq var variable) collect pos)))
                                  (multiple-value-bind (left-hand-type right-hand-types)
                                      (variables->types-backward statements statement top mappings)
                                    (assert right-hand-positions)
                                    (reduce
                                     (alex:curry #'meet top)
                                     (map '(vector type-node)
                                          (lambda (position)
                                            (apply #'apply-argtype-fn db top
                                                   (statement-function statement)
                                                   position left-hand-type
                                                   right-hand-types))
                                          right-hand-positions)))))
                                right-hand-statements))))))))
         mappings)))

;; List of FLAT-NODEs -> WIDE-INFERENCE-RULE
(sera:-> program-wide-forward-rules (list)
         (values wide-inference-rule &optional))
(defun program-wide-forward-rules (graph)
  "Return the forward type inference operator {V -> T}^n -> {V -> T}^n
where n is a number of nodes in the graph GRAPH."
  (let* ((number-of-nodes (length graph))
         (operator (make-array (list number-of-nodes number-of-nodes)
                               :element-type '(or null function)
                               :initial-element nil)))
    (loop for i below number-of-nodes
          for node of-type flat-control-node in graph
          for goes-from = (flat-control-node-from node) do
          (loop for prev-node in goes-from do
                (setf (aref operator i prev-node)
                      (forward-inference-rule
                       (flat-control-node-statements
                        (nth prev-node graph))))))
    operator))

;; List of FLAT-NODEs -> WIDE-INFERENCE-RULE
(sera:-> program-wide-backward-rules (list)
         (values wide-inference-rule &optional))
(defun program-wide-backward-rules (graph)
  "Return the backward type inference operator {V -> T}^n -> {V -> T}^n
where n is a number of nodes in the graph GRAPH."
  (let* ((number-of-nodes (length graph))
         (operator (make-array (list number-of-nodes number-of-nodes)
                               :element-type '(or null function)
                               :initial-element nil)))
    (loop for i below number-of-nodes
          for node of-type flat-control-node in graph
          for goes-to = (flat-control-node-to node) do
          (loop for next-node in goes-to do
                (setf (aref operator i next-node)
                      (backward-inference-rule
                       (flat-control-node-statements
                        (nth i graph))))))
    operator))

(sera:-> constant-program-wide-mapping (type-node symbol list)
         (values wide-vars->types &optional))
(defun constant-program-wide-mapping (top name graph)
  "Return a mapping m^n, m = {v -> Type with name NAME} for all
variables v apperaing in the control flow graph GRAPH and n being
number of nodes in the graph. TOP is the top type of the type system."
  (let ((variables (program-variables graph))
        (bottom (find-type-node name top)))
    (make-array (length graph)
                :element-type 'vars->types
                :initial-element
                (map '(vector var->type)
                       (lambda (var)
                         (cons var bottom))
                       variables))))

(sera:-> mappings-lattice-op (type-node
                              (sera:-> (type-node type-node type-node)
                                       (values type-node &optional))
                              vars->types vars->types)
         (values vars->types &optional))
(defun mappings-lattice-op (top type-lattice-op m1 m2)
  "For mappings m1, m2: {V -> T} where V is a set of variables and T
is a set of types, return a new mapping
m3 = ∀v ∈ V {v -> m1(v) `TYPE-LATTICE-OP` m2(v)}. TOP is the top type
of the type system."
  (map '(vector var->type)
       (lambda (mapping1)
         (destructuring-bind (var . type) mapping1
           (let ((mapping2 (find var m2 :key #'car)))
             (assert mapping2)
             (cons var
                   (funcall type-lattice-op top type (cdr mapping2))))))
       m1))

(sera:-> apply-inference-operator (hash-table
                                   type-node wide-inference-rule
                                   wide-vars->types wide-vars->types)
         (values wide-vars->types &optional))
(defun apply-inference-operator (db top operator safe-mappings mappings)
  "Apply type inference operator to program-wide variable->type mappings."
  (assert (= (length mappings)
             (array-dimension operator 0)
             (array-dimension operator 1)))
  (let ((new-mappings (map '(vector vars->types)
                           (lambda (mapping)
                             (map '(vector var->type)
                                  (lambda (mapping)
                                    (cons (car mapping)
                                          (find-type-node nil top)))
                                  mapping))
                           mappings)))
    (loop for i below (length mappings) do
          (loop for j below (length mappings)
                when (aref operator i j) do
                (setf (aref new-mappings i)
                      (mappings-lattice-op
                       top #'join
                       (aref new-mappings i)
                       (funcall (aref operator i j)
                                db top
                                (aref mappings j))))))
    (map '(vector vars->types)
         (lambda (mapping safe-mapping)
           (mappings-lattice-op top #'meet safe-mapping mapping))
         new-mappings safe-mappings)))

(sera:-> fix-operator (hash-table
                       type-node
                       list
                       wide-inference-rule
                       wide-vars->types)
         (values wide-vars->types &optional))
(defun fix-operator (db top graph operator safe-mappings)
  "Find x such that x = SAFE-MAPPING ∧ OPERATOR(X)"
  (let ((initial-mappings (constant-program-wide-mapping top nil graph)))
    (labels ((%fix (mappings)
               (let ((new-mappings (apply-inference-operator db top operator
                                                             safe-mappings mappings)))
                 (if (equalp mappings new-mappings)
                     mappings
                     (%fix new-mappings)))))
      (%fix initial-mappings))))

(sera:-> inference-pass (hash-table type-node list wide-vars->types)
         (values wide-vars->types &optional))
(defun inference-pass (db top graph safe-mappings)
  (let ((forward-operator  (program-wide-forward-rules  graph))
        (backward-operator (program-wide-backward-rules graph)))
    (reduce
     (lambda (mappings operator)
       (fix-operator db top graph operator mappings))
     (list forward-operator backward-operator)
     :initial-value safe-mappings)))

(sera:-> infer-types (hash-table type-node list)
         (values wide-vars->types &optional))
(defun infer-types (db top graph)
  "Get the best safe program-wide varible->type mappings for a control
flow graph in flat format returned by
@c(flat-control-flow-graph). @c(Graph) is an internal representation
of an expression in the form of parallel assignment statements.
@c(Graph) can be obtained by firstly creating a control flow graph
from an expression by calling @c(parse-code) and secondly converting
it to flat format with @c(flat-control-flow-graph). @c(Db) is a
database of known functions and @c(top) is the top type of your type
system. This function returns an array of variable->type mappings for
each parallel assignment statement in the flattened control flow
graph. The first element in that array is the most important as it
corresponds to a fully evaluated expression."
  (labels ((%fix (mappings)
             (let ((new-mappings (inference-pass db top graph mappings)))
               (if (equalp new-mappings mappings) mappings
                   (%fix new-mappings)))))
    (%fix (constant-program-wide-mapping top t graph))))
