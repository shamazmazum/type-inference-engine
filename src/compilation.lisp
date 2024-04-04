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
    (let* ((types (s/f-node-mappings
                   (infer-types fndb top-type (ir-nodes->flat-nodes nodes))))
           (argtypes (mapcar (alex:curry #'alex:assoc-value types) parameters))
           (restype (alex:assoc-value types result-variable)))

      (let ((bottom (find-type-node nil top-type)))
        (when (find bottom types :key #'cdr)
          (error 'typecheck-error :name name :argtypes argtypes :restype restype)))

    (alex:when-let ((entry (gethash name fndb)))
      (unless (typep entry 'simple-known-function)
        (error 'cannot-redefine :name (known-function-name entry)))
      ;; FIXME: We also do not allow the types to be wider than declared.
      (loop for declared-argtype in (simple-known-function-arg-types entry)
            for actual-argtype   in argtypes
            unless (le (type-node-order top-type actual-argtype declared-argtype)) do
            (error 'typecheck-error :name name :argtypes argtypes :restype restype))
      (unless (le (type-node-order
                   top-type restype
                   (simple-known-function-res-type entry)))
        (error 'typecheck-error :name name :argtypes argtypes :restype restype)))

    (handler-bind
        ((fndb-entry-exists #'fndb-replace))
      (maybe-add-function-to-fndb
       fndb top-type
       (simple-known-function
        name argtypes restype))))))
