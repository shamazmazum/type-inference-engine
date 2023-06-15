(defun do-all()
  (ql:quickload :type-inference-engine/tests)
  (uiop:quit
   (if (uiop:call-function "type-inference-engine/tests:run-tests")
       0 1)))

(do-all)
