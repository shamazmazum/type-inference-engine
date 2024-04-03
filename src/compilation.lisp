(in-package :type-inference-engine)

(sera:-> compile-function (list hash-table type-node &optional list)
         (values simple-known-function &optional))
(defun compile-function (form fndb top-type &optional literal-initializers)
  (multiple-value-bind (nodes name parameters result-variable)
      (parse-defun form literal-initializers)
    (let* ((types (coerce (aref (infer-types
                                 fndb top-type
                                 (ir-nodes->flat-nodes nodes))
                                0)
                          'list))
           (argtypes (mapcar (alex:curry #'alex:assoc-value types) parameters))
           (restype (alex:assoc-value types result-variable))
           (bottom (find-type-node nil top-type)))
      (when (find bottom types :key #'cdr)
        (error 'typecheck-error :name name :argtypes argtypes :restype restype))
      (maybe-add-function-to-fndb
       fndb top-type
       (simple-known-function
        name argtypes restype)))))
