(in-package :type-inference-engine/example)

;; TODO: Functions like FLOOR (?), CEILING (?)
;; and maybe other can be simplified to use SBCL-like DEFKNOWN.

;; NB: The bottom node must have the name NIL
(defparameter *type-system*
  (let* ((bottom   (tie:type-node nil "Bottom type. No value belongs to this type" nil))
         (cons     (tie:type-node 'cons "A pair of any two objects like '(4 . #(1 2 3))"
                                  (list bottom)))
         (null     (tie:type-node 'null "A type with the only value NIL"
                                  (list bottom)))
         (true     (tie:type-node 'true "A type with the only value T"
                                  (list bottom)))
         (list     (tie:type-node 'list "A type for describing a list, both proper and improper"
                                  (list cons null)))
         (vector   (tie:type-node 'vector "A type for one-dimensional arrays"
                                  (list bottom)))
         (sequence (tie:type-node 'sequence "A type for any list-like container"
                                  (list list vector)))
         (array    (tie:type-node 'array "A type for any array of any dimensionality"
                                  (list vector)))
         (integer  (tie:type-node 'integer "A type for integer numbers"
                                  (list bottom)))
         (float    (tie:type-node 'float "A type for floating point numbers (any precision)"
                                  (list bottom)))
         (number   (tie:type-node 'number "A type for numbers"
                                  (list integer float)))
         (boolean  (tie:type-node 'boolean "A type for values T and NIL"
                                  (list true null))))
    (tie:check-type-system
     (tie:type-node t "A type for any value" (list sequence array number boolean))))
  "Type system of my toy language")

(defparameter *fndb* (tie:make-fndb *type-system*)
  "A database for known functions")

;; 1+ function with adds 1 to any number. In Haskell notation:
;; 1+ :: Num a => a -> a
(tie:defknown *fndb* *type-system* (1+ 1-) ((x) (res top n))
  ((number . number)
   (bottom . nil))
  (:bottom-guard bottom)
  ;; T₀
  (((tie:le (tie:type-node-order top x number)) x)
   ((tie:ge (tie:type-node-order top x number)) number))
  ;; T₁
  (((and (tie:types-intersect-p top res number)
         (tie:types-intersect-p top x   number))
    ;; t1 ∧ t2 ∧ NUMBER
    (tie:meet top (tie:meet top res x) number))))

;; sin :: Num a => a -> NUMBER
(tie:defknown *fndb* *type-system* (sin cos) ((x) (res top n))
  ((number . number)
   (float  . float)
   (bottom . nil))
  (:bottom-guard bottom)
  ;; T₀
  (((tie:ge (tie:type-node-order top x number)) number)
   ;; INTEGER / FLOAT, because NIL is already checked
   ((tie:le (tie:type-node-order top x number)) float))
  ;; T₁
  ;; FLOAT / NUMBER / T is OK for the result
  (((tie:ge (tie:type-node-order top res float))
    (tie:meet top x number))))

;; length :: Seq a => a -> INTEGER
(tie:defknown* *fndb* *type-system* (length) (sequence) integer)

;; Plus / minus
(tie:defknown *fndb* *type-system* (* + -) ((x y) (res top n))
  ((number  . number)
   (integer . integer)
   (float   . float)
   (bottom  . nil))
  (:bottom-guard bottom)
  ;; T₀
  (((or (and (eq x integer)
             (eq y float))
        (and (eq y integer)
             (eq x float)))
    ;; Type conversion rule: INTEGER ± FLOAT = FLOAT
    float)
   ;; Other combination of numeric types
   ((and (tie:le (tie:type-node-order top x number))
         (tie:le (tie:type-node-order top y number)))
    (tie:join top x y))
   ;; Combinations with T
   ((and (tie:types-intersect-p top x number)
         (tie:types-intersect-p top y number))
    number))
  ;; T₁ / T₂
  ;; Can be improved, I think
  (((and (tie:types-intersect-p top x number)
         (tie:types-intersect-p top y number)
         (tie:ge (tie:type-node-order top res float)))
    (tie:meet top number
              (ecase n (0 x) (1 y))))
   ((eq res integer)
    (tie:meet top integer
              (tie:meet top x y)))))

;; Floor / ceiling
;; floor   :: Num a => a -> INTEGER
;; ceiling :: Num a => a -> INTEGER
(tie:defknown* *fndb* *type-system* (floor ceiling) (number) integer)

;; ELT
(tie:defknown* *fndb* *type-system* (elt) (sequence integer) t)

;; NUMBERP
(tie:defknown *fndb* *type-system* (numberp) ((x) (res top n))
  ((boolean . boolean)
   (null    . null)
   (true    . true)
   (number  . number)
   (bottom  . nil))
  (:bottom-guard bottom)
  ;; T₀
  (((eq x top) boolean)
   ((tie:le (tie:type-node-order top x number)) true)
   (t null))
  ;; T₁
  ;; TODO: Is it possible to handle res = NULL?
  (((eq res true)  (tie:meet top x number))
   ((tie:ge (tie:type-node-order top res null)) x)))

;; NOT
(tie:defknown *fndb* *type-system* (not) ((x) (res top n))
  ((boolean . boolean)
   (null    . null)
   (true    . true)
   (bottom  . nil))
  (:bottom-guard bottom)
  ;; T₀
  (((and (tie:ge (tie:type-node-order top x null))
         (eq (tie:meet top x boolean) null))
    true)
   ((eq x true) null)
   ((tie:ge (tie:type-node-order top x boolean)) boolean))
  ;; T₁
  (((eq res true)
    (tie:meet top x null))
   ((eq res null)
    (tie:meet top x true))
   ((tie:types-intersect-p top res boolean)
    (tie:meet top x boolean))))

(tie:defknown* *fndb* *type-system* (=) (number number) boolean)

;; bool :: BOOLEAN -> a -> b -> (or a b)
(tie:defknown *fndb* *type-system* (bool) ((c then else) (res top n))
  ((boolean . boolean)
   (null    . null)
   (true    . true)
   (bottom  . nil))
  (:bottom-guard bottom)
  ;; T₀
  (((eq c true) then)
   ((eq c null) else)
   ((tie:types-intersect-p top c boolean)
    (tie:join top then else)))
  ;; T₁/T₂/T₃
  ((t
    (ecase n
      (0 (tie:meet top c boolean))
      (1 (if (eq c true) (tie:meet top res then) then))
      (2 (if (eq c null) (tie:meet top res else) else))))))

;; cons :: a -> b -> CONS
(tie:defknown* *fndb* *type-system* (cons) (t t) cons)


;; Literals

(tie:definitializer *fndb* *type-system* init/integer integer)
(tie:definitializer *fndb* *type-system* init/float   float)
(tie:definitializer *fndb* *type-system* init/null    null)
(tie:definitializer *fndb* *type-system* init/true    true)
(tie:definitializer *fndb* *type-system* init/cons    cons)
(tie:definitializer *fndb* *type-system* init/vector  vector)
(tie:definitializer *fndb* *type-system* init/array   array)

(defun quoted-cons-p (code)
  (if (listp code)
      (destructuring-bind (q cons &rest rest) code
        (and
         (eq q 'quote)
         (consp cons)
         (null rest)))))

(defparameter *literal-initializers*
  (list
   (cons #'integerp 'init/integer)
   (cons #'floatp   'init/float)
   (cons #'null     'init/null)
   (cons (alex:curry #'eq t)
                    'init/true)
   (cons #'quoted-cons-p
                    'init/cons)
   (cons #'vectorp  'init/vector)
   (cons #'arrayp   'init/array))
  "Associative list of predicates for literals (keys) and init
functions for variables (vals).")

;; Inference
(defun infer-types (expression)
  "Infer types in the expression using a top type system and function
database."
  (multiple-value-bind (nodes result-variable)
      (tie:parse-code expression *literal-initializers*)
    (values
     (elt (tie:infer-types *fndb* *type-system* nodes) 0)
     result-variable)))

(defun compile-function (form)
  (tie:compile-function form *fndb* *type-system* *literal-initializers*))
