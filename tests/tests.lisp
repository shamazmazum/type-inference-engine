(in-package :type-inference-engine/tests)

(def-suite graph          :description "Graph traversal tests")
(def-suite type-system    :description "Type-system tests")
(def-suite type-inference :description "Type inference rules")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(graph type-system type-inference))))

(in-suite graph)
(defun fold-list (fn initial-value list &key circularp)
  (tie:fold-graph
   list
   (sera:hook2 fn #'car)
   initial-value
   (alex:compose #'list #'cdr)
   circularp))

(sera:defconstructor binary-tree
  (value integer)
  (left-node  (or null binary-tree))
  (right-node (or null binary-tree)))

(defun fold-binary-tree (fn initial-value tree)
  (tie:fold-graph
   tree
   (sera:hook2 fn #'binary-tree-value)
   initial-value
   (sera:fork #'list
              #'binary-tree-left-node
              #'binary-tree-right-node)))

(defun find-node (node-value tree)
  (tie:fold-graph
   tree
   (lambda (acc x)
     (declare (ignore acc))
     (if (= (binary-tree-value x) node-value)
         (values x   t)
         (values nil nil)))
   nil
   (sera:fork #'list
              #'binary-tree-left-node
              #'binary-tree-right-node)))

(test fold-list
  (is (= (fold-list #'+ 0 '(1 2 3 4)) 10))
  (signals tie:cycle-detected (fold-list #'+ 0 '#1=(1 2 3 4 . #1#)))
  (is (= (fold-list #'+ 0 '#2=(1 2 3 4 . #2#) :circularp t))))

(defparameter *binary-tree*
  (let* ((node1 (binary-tree 10 nil nil))
         (node2 (binary-tree 1 node1 nil))
         (node3 (binary-tree 12 nil nil))
         (node4 (binary-tree 16 nil nil))
         (node5 (binary-tree 16 node3 node4))
         (node6 (binary-tree 100 node2 node5)))
    node6))

(test fold-binary-tree
  (is (= (fold-binary-tree #'+ 0 *binary-tree*) 155))
  (let ((node (find-node 1 *binary-tree*)))
    (is (= (binary-tree-value node) 1))
    (is (null (binary-tree-right-node node)))
    (is (= (binary-tree-value
            (binary-tree-left-node node))
           10))))

(in-suite type-system)
(test meet-and-join-properties
  (let ((types (tie:flatten-type-graph tie/ex:*type-system*)))
    (loop for t1 in types do
          (loop for t2 in types do
                (is (eq (tie:meet tie/ex:*type-system* t1 t2)
                        (tie:meet tie/ex:*type-system* t2 t1)))
                (is (eq (tie:join tie/ex:*type-system* t1 t2)
                        (tie:join tie/ex:*type-system* t2 t1)))
                (when (eq t1 (tie:join tie/ex:*type-system* t1 t2))
                  (is (eq t2 (tie:meet tie/ex:*type-system* t1 t2))))
                (when (eq t1 (tie:meet tie/ex:*type-system* t1 t2))
                  (is (eq t2 (tie:join tie/ex:*type-system* t1 t2))))
                (when (eq :lt (tie:type-node-order tie/ex:*type-system* t1 t2))
                  (is (eq :gt (tie:type-node-order tie/ex:*type-system* t2 t1))))
                (when (eq :gt (tie:type-node-order tie/ex:*type-system* t1 t2))
                  (is (eq :lt (tie:type-node-order tie/ex:*type-system* t2 t1))))))))

(in-suite type-inference)
(test forward-inference
  (let* ((statements
          (list (tie:statement 'l1 '1+     '(r1))
                (tie:statement 'l2 '1+     '(r2))
                (tie:statement 'l3 'length '(r3))
                (tie:statement 'l4 '1+     '(r4))))
         (rule (tie:forward-inference-rule statements))

         (bottom   (tie:find-type-node nil       tie/ex:*type-system*))
         (top      (tie:find-type-node t         tie/ex:*type-system*))
         (integer  (tie:find-type-node 'integer  tie/ex:*type-system*))
         (number   (tie:find-type-node 'number   tie/ex:*type-system*))
         (sequence (tie:find-type-node 'sequence tie/ex:*type-system*))
         (list     (tie:find-type-node 'list     tie/ex:*type-system*))

         (mappings-before (make-array 9
                                      :element-type 'tie:var->type
                                      :initial-contents
                                      `((r1 . ,integer)
                                        (r2 . ,number)
                                        (r3 . ,top)
                                        (r4 . ,list)

                                        (l1 . ,bottom)
                                        (l2 . ,bottom)
                                        (l3 . ,bottom)
                                        (l4 . ,number)

                                        (tmp . ,list))))
         (mappings-after (funcall rule tie/ex:*fndb* tie/ex:*type-system*
                                  mappings-before)))
    (is (equalp (aref mappings-after 0) (cons 'r1 integer)))
    (is (equalp (aref mappings-after 1) (cons 'r2 number)))
    (is (equalp (aref mappings-after 2) (cons 'r3 sequence)))
    (is (equalp (aref mappings-after 3) (cons 'r4 bottom)))

    (is (equalp (aref mappings-after 4) (cons 'l1 integer)))
    (is (equalp (aref mappings-after 5) (cons 'l2 number)))
    (is (equalp (aref mappings-after 6) (cons 'l3 integer)))
    (is (equalp (aref mappings-after 7) (cons 'l4 bottom)))

    (is (equalp (aref mappings-after 8) (cons 'tmp list)))))

(test backward-inference
  (let* ((statements
          (list (tie:statement 'l1 '1+     '(r1))
                (tie:statement 'l2 '1+     '(r2))
                (tie:statement 'l3 'length '(r3))
                (tie:statement 'r4 '1+     '(r4))
                (tie:statement 'l4 '+     '(r5 r6))))
         (rule (tie:backward-inference-rule statements))

         (top      (tie:find-type-node t         tie/ex:*type-system*))
         (integer  (tie:find-type-node 'integer  tie/ex:*type-system*))
         (number   (tie:find-type-node 'number   tie/ex:*type-system*))
         (sequence (tie:find-type-node 'sequence tie/ex:*type-system*))
         (list     (tie:find-type-node 'list     tie/ex:*type-system*))

         (mappings-before (make-array 11
                                      :element-type 'tie:var->type
                                      :initial-contents
                                      `((r1 . ,top)
                                        (r2 . ,top)
                                        (r3 . ,top)
                                        (r4 . ,integer)
                                        (r5 . ,number)
                                        (r6 . ,number)

                                        (l1 . ,top)
                                        (l2 . ,integer)
                                        (l3 . ,top)
                                        (l4 . ,integer)

                                        (tmp . ,list))))
         (mappings-after (funcall rule tie/ex:*fndb* tie/ex:*type-system*
                                  mappings-before)))

    (is (equalp (aref mappings-after 0) (cons 'r1 number)))
    (is (equalp (aref mappings-after 1) (cons 'r2 integer)))
    (is (equalp (aref mappings-after 2) (cons 'r3 sequence)))
    (is (equalp (aref mappings-after 3) (cons 'r4 integer)))
    (is (equalp (aref mappings-after 4) (cons 'r5 integer)))
    (is (equalp (aref mappings-after 5) (cons 'r6 integer)))

    (is (equalp (aref mappings-after 6) (cons 'l1 top)))
    (is (equalp (aref mappings-after 7) (cons 'l2 top)))
    (is (equalp (aref mappings-after 8) (cons 'l3 top)))
    (is (equalp (aref mappings-after 9) (cons 'l4 top)))

    (is (equalp (aref mappings-after 10) (cons 'tmp list)))))

(defun var-type (mappings var)
  (cdr (find var mappings :key #'car)))

(defun find-type (name)
  (tie:find-type-node name tie/ex:*type-system*))

(test wide-inference-1
  (multiple-value-bind (res-mappings res-var)
      (tie/ex:infer-types '(+ (sin x) (- y (length x))))
    (is (eq (var-type res-mappings res-var)
            (find-type nil)))
    (is (eq (var-type res-mappings 'y)
            (find-type nil)))
    (is (eq (var-type res-mappings 'x)
            (find-type nil)))))

(test wide-inference-2
  (multiple-value-bind (res-mappings res-var)
      (tie/ex:infer-types '(+ (floor (elt x y)) (1+ y)))
    (is (eq (var-type res-mappings res-var)
            (find-type 'integer)))
    (is (eq (var-type res-mappings 'y)
            (find-type 'integer)))
    (is (eq (var-type res-mappings 'x)
            (find-type 'sequence)))))

(test wide-inference-3
  (multiple-value-bind (res-mappings res-var)
      (tie/ex:infer-types '(sin (elt x x)))
    (is (eq (var-type res-mappings res-var)
            (find-type nil)))
    (is (eq (var-type res-mappings 'x)
            (find-type nil)))))

(test wide-inference-4
  (multiple-value-bind (res-mappings res-var)
      (tie/ex:infer-types '(sin (elt x (+ y z))))
    (is (eq (var-type res-mappings res-var)
            (find-type 'number)))
    (is (eq (var-type res-mappings 'x)
            (find-type 'sequence)))
    (is (eq (var-type res-mappings 'y)
            (find-type 'integer)))
    (is (eq (var-type res-mappings 'z)
            (find-type 'integer)))))

(test branching-1
  (multiple-value-bind (res-mappings res-var)
      (tie/ex:infer-types '(if (numberp x) (sin x) y))
    ;; IF evaluates its arguments, so X HAVE to be number
    (is (eq (var-type res-mappings res-var)
            (find-type 'number)))
    (is (eq (var-type res-mappings 'x)
            (find-type 'number)))
    (is (eq (var-type res-mappings 'y)
            (find-type t)))))

(test branching-2
  (multiple-value-bind (res-mappings res-var)
      (tie/ex:infer-types '(if (numberp x) (sin y) (cons x y)))
    ;; Both branches are possible
    (is (eq (var-type res-mappings res-var)
            (find-type t)))
    (is (eq (var-type res-mappings 'x)
            (find-type t)))
    (is (eq (var-type res-mappings 'y)
            (find-type 'number)))))

(test literals-1
  (multiple-value-bind (res-mappings res-var)
      (tie/ex:infer-types '(+ 5.0 (elt x (+ y 4))))
    (is (eq (var-type res-mappings res-var)
            (find-type 'number)))
    (is (eq (var-type res-mappings 'y)
            (find-type 'integer)))
    (is (eq (var-type res-mappings 'x)
            (find-type 'sequence)))))

(test literals-2
  (multiple-value-bind (res-mappings res-var)
      (tie/ex:infer-types '(+ 5.0 (elt x (+ y 1.4))))
    (is (eq (var-type res-mappings res-var)
            (find-type nil)))
    (is (eq (var-type res-mappings 'y)
            (find-type nil)))
    (is (eq (var-type res-mappings 'x)
            (find-type nil)))))

(test literals-3
  (multiple-value-bind (res-mappings res-var)
      (tie/ex:infer-types '(+ 3.5 (floor (sin x))))
    (is (eq (var-type res-mappings res-var)
            (find-type 'float)))
    (is (eq (var-type res-mappings 'x)
            (find-type 'number)))))
