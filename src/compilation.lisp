(in-package :type-inference-engine)

(sera:-> s/f-node-mappings (wide-vars->types)
         (values list &optional))
(declaim (inline s/f-node-mappings))
(defun s/f-node-mappings (mappings)
  (coerce (aref mappings 0) 'list))

(sera:-> compile-function (list hash-table type-node &optional list)
         (values simple-known-function &optional))
(defun compile-function (form fndb top-type &optional literal-initializers)
  "Infer types for a function defined by the defun form (just like in
Common Lisp) and add this function to the database if it
typechecks.

Example (@c(tie/ex) is a local nickname for the package
@c(type-inference-engine/example)):
@begin[lang=lisp](code)
TIE-EXAMPLE> (tie:compile-function
 '(defun foo (x y)
   (let ((z (elt x (1+ y))))
     (* z (+ y z))))
  tie/ex:*fndb* tie/ex:*type-system*)
#S(TIE::SIMPLE-KNOWN-FUNCTION
   :NAME FOO
   :ARG-TYPES (#<TIE:TYPE-NODE SEQUENCE {102E78D253}>
               #<TIE:TYPE-NODE INTEGER {102E78D2B3}>)
   :RES-TYPE #<TIE:TYPE-NODE NUMBER {102E78D313}>)
@end(code)

See also: @c(parse-expr)."
  (multiple-value-bind (nodes name parameters)
      (parse-defun fndb top-type form literal-initializers)
    (let* ((mappings (s/f-node-mappings
                      (infer-types fndb top-type nodes)))
           (result-variable (result-variable nodes))
           (actual-argtypes (mapcar (alex:curry #'alex:assoc-value mappings) parameters))
           (actual-restype (alex:assoc-value mappings result-variable)))

      ;; Check that every variable contains a value
      (loop with bottom = (find-type-node nil top-type)
            for mapping in mappings
            when (eq bottom (cdr mapping)) do
            (error 'typecheck-error :name name :varname (car mapping) :actual (cdr mapping)))

      ;; FIXME: We also do not allow the types to be wider than declared.
      (alex:when-let ((entry (gethash name fndb)))
        (unless (typep entry 'simple-known-function)
          (error 'cannot-redefine :name (known-function-name entry)))
        (loop for declared-argtype in (simple-known-function-arg-types entry)
              for actual-argtype   in actual-argtypes
              for parameter        in parameters
              unless (le (type-node-order top-type actual-argtype declared-argtype)) do
            (error 'typecheck-error
                   :name     name
                   :varname  parameter
                   :actual   actual-argtype
                   :expected declared-argtype))
        (let ((declared-restype (simple-known-function-res-type entry)))
          (unless (le (type-node-order top-type actual-restype declared-restype))
            (error 'typecheck-error
                   :name     name
                   :varname  result-variable
                   :actual   actual-restype
                   :expected declared-restype))))

      ;; Update FNDB
      (handler-bind
          ((fndb-entry-exists #'fndb-replace))
        (maybe-add-function-to-fndb
         fndb top-type (simple-known-function name actual-argtypes actual-restype))))))
