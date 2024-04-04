(in-package :type-inference-engine)

(sera:-> s/f-node-mappings (wide-vars->types)
         (values list &optional))
(declaim (inline s/f-node-mappings))
(defun s/f-node-mappings (mappings)
  (coerce (aref mappings 0) 'list))

(sera:-> compile-function (list hash-table type-node &optional list)
         (values simple-known-function &optional))
(defun compile-function (form fndb top-type &optional literal-initializers)
  (multiple-value-bind (nodes name parameters result-variable)
      (parse-defun fndb top-type form literal-initializers)
    (let* ((mappings (s/f-node-mappings
                      (infer-types fndb top-type (ir-nodes->flat-nodes nodes))))
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
