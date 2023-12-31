(in-package :type-inference-engine)

(sera:defconstructor known-function
  (name       symbol)
  (arity      alex:non-negative-fixnum)
  (restype-fn function)
  (argtype-fn (or null function)))

(defparameter *fndb* (make-hash-table)
  "A database of known primitives for our language")

(sera:-> get-db-entry (hash-table symbol list)
         (values known-function &optional))
(defun get-db-entry (db name arguments)
  (let ((db-entry (gethash name db)))
    (unless db-entry
      (error 'unknown-function :name name))
    (unless (= (length arguments)
               (known-function-arity db-entry))
      (error 'arity-error
             :name     name
             :actual   (length arguments)
             :expected (known-function-arity db-entry)))
    db-entry))

(sera:-> apply-restype-fn (hash-table type-node symbol &rest type-node)
         (values type-node &optional))
(defun apply-restype-fn (db top name &rest arg-types)
  "Apply forward inference function for a primitive function NAME and
argument types ARG-TYPES. DB is a function database and TOP is the top
type of a type system."
  (let* ((db-entry (get-db-entry db name arg-types))
         (restype-fn (known-function-restype-fn db-entry)))
    (apply restype-fn top arg-types)))

(sera:-> apply-argtype-fn (hash-table type-node symbol alex:non-negative-fixnum type-node
                                      &rest type-node)
         (values type-node &optional))
(defun apply-argtype-fn (db top name narg res-type &rest arg-types)
  "Apply NARG-th backward inference function for a primitive function
NAME, result type RES-TYPE and argument types ARG-TYPES. DB is a
function database and TOP is the top type of a type system."
  (let* ((db-entry (get-db-entry db name arg-types))
         (argtype-fn (known-function-argtype-fn db-entry)))
    (apply argtype-fn top narg res-type arg-types)))

;; For testing
(sera:-> unary-function-monotonic-p
         (type-node
          (sera:-> (type-node) (values type-node &optional)))
         (values boolean &optional))
(defun unary-function-monotonic-p (top fn)
  "Check if an unary function TYPE -> TYPE is monotonic."
  (every
   #'identity
   (loop for tails on (flatten-type-graph top)
         for t1 = (car tails) append
         (loop for t2 in tails collect
               (let* ((arg-order (type-node-order top t1 t2))
                      (res-order (type-node-order top
                                                  (funcall fn t1)
                                                  (funcall fn t2)))
                      (monotonicp (or
                                   ;; t1 ≤ t2 and f(t1) ≤ f(t2)
                                   (and (le arg-order)
                                        (le res-order))
                                   ;; t1 ≥ t2 and f(t1) ≥ f(t2)
                                   (and (ge arg-order)
                                        (ge res-order))
                                   ;; No order defined
                                   (null arg-order))))
                 (unless monotonicp
                   (warn "Function ~a is not monotonic for types: ~a, ~a~%"
                         fn t1 t2))
                 monotonicp)))))

(sera:-> type-space^n (type-node alexandria:non-negative-fixnum)
         (values si:iterator &optional))
(defun type-space^n (top n)
  "Return iterator which iterates through all elements of set T^n
where T is a set of types with TOP beign the top type."
  (if (zerop n)
      (si:list->iterator nil)
      (let* ((all-types (si:list->iterator (flatten-type-graph top)))
             (iterators (loop repeat n collect all-types)))
        (si:imap #'alex:flatten
                 (reduce #'si:product iterators)))))

(defun insert-at (list elt idx)
  (append
   (subseq list 0 idx)
   (cons elt (subseq list idx))))

(sera:-> function-monotonic-p
         ;; FIXME: Cannot express a type for n-ary function :(
         (type-node alexandria:positive-fixnum function)
         (values boolean &optional))
(defun function-monotonic-p (top arity fn)
  "Return T if function FN of arity ARITY is monotonic on all of its
arguments."
  (if (= arity 1)
      (unary-function-monotonic-p top fn)
      (let ((type-space (type-space^n top (1- arity))))
        (every
         #'identity
         (loop for i below arity append
               (si:collect
                   (si:imap
                    (lambda (fixed-types)
                      (unary-function-monotonic-p
                       top
                       (lambda (x)
                         (let ((arguments (insert-at fixed-types x i)))
                           (apply fn arguments)))))
                    type-space)))))))

(sera:-> unary-function-narrowing-p
         (type-node
          (sera:-> (type-node) (values type-node &optional)))
         (values boolean &optional))
(defun unary-function-narrowing-p (top fn)
  "Return T if FN(x) ≤ x for ∀x ≤ TOP."
  (let* ((arg-space (flatten-type-graph top))
         (val-space (mapcar (alex:curry #'funcall fn) arg-space)))
    (every
     (lambda (arg val)
       (let ((le (le (type-node-order top val arg))))
         (unless le
           (warn "Function ~a is not narrowing for type ~a"
                 fn arg))
         le))
     arg-space val-space)))

(sera:-> function-narrowing-p
         ;; FIXME: Cannot express a type for n-ary function :(
         (type-node alexandria:positive-fixnum alexandria:non-negative-fixnum function)
         (values boolean &optional))
(defun function-narrowing-p (top arity arg fn)
  "Return T if function FN(x_0, x_1, …, x_ARG, …, x_{ARITY-1}) ≤ x_ARG
for all x_i ∈ Set of types."
  (let ((type-space (type-space^n top (1- arity))))
        (si:foldl
         (lambda (x y) (and x y)) t
         (si:imap
          (lambda (fixed-types)
            (unary-function-narrowing-p
             top
             (lambda (x)
               (let ((arguments (insert-at fixed-types x arg)))
                 (apply fn arguments)))))
          type-space))))

(sera:-> known-function-correct-p (type-node known-function)
         (values boolean &optional))
(defun known-function-correct-p (top function)
  "Return T if a knwon function FUNCTION is correctly defined."
  (let ((arity (known-function-arity function)))
    (if (not (zerop arity))
        (and
         ;; T_0 must be monotone on every argument
         (function-monotonic-p
          top arity
          (alex:curry (known-function-restype-fn function) top))
         ;; T_i, i > 0 must be monotone on every argument
         (every
          #'identity
          (loop for i below arity collect
                (function-monotonic-p
                 top (1+ arity)
                 (alex:curry (known-function-argtype-fn function) top i))))
         ;; T_i, i > 0 must narrow i-th argument
         (every
          #'identity
          (loop for i below arity collect
                (function-narrowing-p
                 top (1+ arity) (1+ i)
                 (alex:curry (known-function-argtype-fn function) top i)))))
        ;; Nothing to check
        t)))

(sera:-> maybe-add-function-to-fndb (hash-table type-node known-function)
         (values known-function &optional))
(defun maybe-add-function-to-fndb (db top function)
  "Add a known information about FUNCTION to a database DB. If
FUNCTION is already in the database, signal a warning. TOP is the top
type of used type system."
  (unless (known-function-correct-p top function)
    (error 'incorrect-definition
           :name (known-function-name function)))
  (with-simple-restart (fndb-abort "Abort insertion to fndb")
    (let ((name (known-function-name function)))
      (with-simple-restart (fndb-replace "Replace the old definition from fndb")
          (when (gethash name db)
            (error 'fndb-entry-exists :name name)))
      (setf (gethash name db) function)))
  function)

(defmacro %defknown (db top name
                     (argument-vars
                      (res-var top-var narg-var))
                     type-bindings
                     (&key (bottom-guard nil bottom-guard-p))
                     t0-cond-clauses &optional t1+-cond-clauses)
  (unless bottom-guard-p
    (error "Specify :bottom-guard"))
  (alex:with-gensyms (%top)
    `(let* ((,%top ,top)
            ,@(loop for (type-var . type-name) in type-bindings collect
                    `(,type-var (find-type-node ',type-name ,%top))))
       (maybe-add-function-to-fndb
        ,db ,%top
        (known-function
         ',name ,(length argument-vars)
         (lambda ,(cons top-var argument-vars)
           (declare (ignorable ,top-var))
           (cond
             ,@(if bottom-guard
                   `(((or ,@(loop for var in argument-vars collect
                                  `(eq ,var ,bottom-guard)))
                      ,bottom-guard)))
             ,@t0-cond-clauses
             ,@(if bottom-guard
                   `((t ,bottom-guard)))))
         ,(if t1+-cond-clauses
              `(lambda ,(list* top-var narg-var res-var argument-vars)
                 (declare (ignorable ,top-var))
                 (assert (< ,narg-var ,(length argument-vars)))
                 (cond
                   ,@(if bottom-guard
                         `(((or (eq ,res-var ,bottom-guard)
                                ,@(loop for var in argument-vars collect
                                        `(eq ,var ,bottom-guard)))
                            ,bottom-guard)))
                   ,@t1+-cond-clauses
                   ,@(if bottom-guard
                   `((t ,bottom-guard)))))))))))

(defmacro defknown (db top names
                    arguments-clause type-bindings-clause
                    (&key (bottom-guard nil bottom-guard-p))
                    t0-cond-clauses &optional t1+-cond-clauses)
  "Add new known functions to the database @c(db). @c(Top) is the top
type of a type system. @c(Names) is a list of function names to be
added. The new functions all have the same arity and the same
functions T₀, T₁, etc.

@c(Type-bindings-clause) is a form @c((arglist (result-variable
top-variable narg-variable))) and contains some important variables
which can be used in the body of functions T_i. @c(Arglist) is a list
of variables which are bound to types of arguments of the defined
functions. @c(Top-variable) is bound to the top type of a type-system,
@c(res-variable) is bound to type of the result of the defined
function and @c(narg-variable) is bound to i-1 in the body of T_i when
i > 0.

@c(Type-bindings-clause) is an associative list with entries in the
form @c((variable-name . type-name)) which contains additional
bindings of variables to types inside T_i functions.

@c(Bottom-guard) argument must always be present and be either @c(nil)
or name of a variable bound to the bottom type. If @c(bottom-guard) is
not null then bottom guards will be present in T_i functions.

@c(T0-cond-clauses) define a body of function T₀ in which bindings to
@c(top-variable) and variables in @c(arglist) are in effect. This body
is an explicit cond, so @c(t0-cond-clauses) may be like this
@begin[lang=lisp](code)
((condition-1 form-1)
 (condition-2 form-2)
 etc)
@end(code)
@c(T0-cond-clauses) must evaluate to a type. If @c(bottom-guard) is
not @c(nil) an implicit conditional is inserted @i(before) all other
conditionals in cond. This conditional checks if any of variables in
@c(arglist) are bound to the bottom type, and if so the bottom type is
returned. Also another conditional is inserted @i(after) all other
conditionals which just returns the bottom type in any case.

@c(T1+-cond-clauses) define a body of functions T₁, T₂, etc in which
bindings to @c(top-variable), @c(res-variable), @c(narg-variable) and
variables in @c(arglist) are in effect. This body is an explicit cond
and must evaluate to a type, just like @c(T0-cond-clauses).

For example the following code
@begin[lang=lisp](code)
(defknown *fndb* *type-system* (length) ((x) (res top n))
  ((integer  . integer)
   (sequence . sequence)
   (bottom   . nil))
  (:bottom-guard bottom)
  ;; T₀
  (((types-intersect-p top x sequence) integer))
  ;; T₁
  (((ge (type-node-order top res integer))
    (meet top x sequence))))
@end(code)
creates a function T₀
@begin[lang=lisp](code)
(lambda (top x)
  (let ((integer  (find-type-node 'integer  top))
        (sequence (find-type-node 'sequence top))
        (bottom   (find-type-node nil       top)))
    (cond
      ((eq x bottom) bottom)
      ((types-intersect-p top x sequence) integer)
      (t bottom))))
@end(code)
and T₁
@begin[lang=lisp](code)
(lambda (top n res x)
  (assert (< n 1))
  (let ((integer  (find-type-node 'integer  top))
        (sequence (find-type-node 'sequence top))
        (bottom   (find-type-node nil       top)))
    (cond
      ((or (eq x   bottom)
           (eq res bottom))
       bottom)
      ((ge (type-node-order top res integer))
       (meet top x sequence))
      (t bottom))))
@end(code)
and adds a corresponding entry about the function @c(length) in
@c(*fndb*)."
  (unless bottom-guard-p
    (error "Specify :bottom-guard"))
  (let ((db-var  (gensym))
        (top-var (gensym)))
    `(let ((,db-var  ,db)
           (,top-var ,top))
       ,@(loop for name in names collect
               `(%defknown ,db-var ,top-var ,name ,arguments-clause ,type-bindings-clause
                           (:bottom-guard ,bottom-guard)
                           ,t0-cond-clauses ,t1+-cond-clauses)))))

(defmacro definitializer (db top name type)
  "Add a function with the name @c(name) which takes zero arguments
and returns a value of type @c(type) to FNDB @c(db)
@begin[lang=lisp](code)
(definitializer db top name type)
@end(code)
is equivalent to
@begin[lang=lisp](code)
(defknown db top (name) (() res top n)
  ((type-var . type))
  (:bottom-guard nil)
  ((t type-var))
  ())
@end(code)"
  (alex:with-gensyms (res-var top-var n-var type-var)
    `(defknown ,db ,top (,name) (() (,res-var ,top-var ,n-var))
       ((,type-var . ,type))
       (:bottom-guard nil)
       ((t ,type-var))
       ())))

(sera:-> make-fndb (type-node)
         (values hash-table &optional))
(defun make-fndb (top-node)
  "Make a functions database for a type system with the top node
@c(top-node)."
  (let ((fndb (make-hash-table)))
    ;; Add mandatory functions

    ;; Initializer pseudo-function. Used for initialization of
    ;; variables with any values of type T.
    (definitializer fndb top-node initialize t)

    ;; Identity function T -> T
    (defknown fndb top-node (identity) ((x) (res top n))
      ()
      (:bottom-guard nil)
      ;; T₀
      ((t x))
      ;; T₁
      ((t (meet top res x))))

    fndb))
