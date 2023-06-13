(defpackage type-inference-engine/example
  (:use #:cl)
  (:local-nicknames (#:tie  #:type-inference-engine)
                    (#:alex #:alexandria))
  (:export #:*type-system*
           #:*fndb*
           #:*literal-initializers*
           #:infer-types

           ;; Extra functions whose symbols are not in CL package
           #:bool))
