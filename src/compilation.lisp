(in-package :type-inference-engine)

(sera:defconstructor function-info
  (name     symbol)
  (argtypes list)
  (restype  type-node)
  (top      type-node))

(defmethod print-object ((info function-info) stream)
  (print-unreadable-object (info stream :type t)
    (format stream "~a :: ~a → ~a"
            (function-info-name info)
            (mapcar #'type-node-name (function-info-argtypes info))
            (type-node-name (function-info-restype info)))))

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
           (bottom (find-type-node nil top-type))
           (arity (length parameters)))
      (when (find bottom types :key #'cdr)
        (error 'typecheck-error :name name :argtypes argtypes :restype restype))
      (maybe-add-function-to-fndb
       fndb top-type
       (extended-known-function
        name arity
        (lambda (top-type &rest actual-argtypes)
          (cond
            ((some (alex:curry #'eq bottom) actual-argtypes) bottom)
            ((every (alex:curry #'types-intersect-p top-type) argtypes actual-argtypes)
             restype)
            (t bottom)))
        (if (not (zerop arity))
            (lambda (top-type arg actual-restype &rest actual-argtypes)
              (cond
                ((some (alex:curry #'eq bottom) actual-argtypes) bottom)
                ((and (every (alex:curry #'types-intersect-p top-type)
                             argtypes actual-argtypes)
                      (types-intersect-p top-type restype actual-restype))
                 (meet top-type (nth arg argtypes) (nth arg actual-argtypes)))
                (t bottom)))))
       t)
      (function-info name argtypes restype top-type))))
