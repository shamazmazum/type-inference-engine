(defpackage type-inference-engine/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:tie    #:type-inference-engine)
                    (#:tie/ex #:type-inference-engine/example)
                    (#:sera   #:serapeum)
                    (#:alex   #:alexandria))
  (:export #:run-tests))
