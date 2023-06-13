(in-package :type-inference-engine)

(defun fold-graph (node function initial-value next-nodes &optional circularp)
  "Fold a graph NODE using a function FUNCTION and initial value
INITIAL-VALUE. FUNCTION must take two arguments: the first is an
accumulator, and the second is the current node of the graph. This
function returns a new value of accumulator and an optional boolean
value. The boolean value stops folding after processing the current
node if it's equal to T. NEXT-NODES is a function which takes a node
and returns a list of subnodes of this node. Folding is done in the
following order: firstly FUNCTION is applied to NODE and then to its
subnodes from the first to the last. If a cycle is detected,
FOLD-GRAPH signals CYCLE-DETECTED condition unless CIRCULARP is T. In
this case any node which was seen before is ignored."
  (labels ((%fold (seen acc current-node)
             (cond
               ((member current-node seen)
                (if circularp acc
                    (error 'cycle-detected :node node)))
               ((null current-node) acc)
               (t
                (let ((seen (cons current-node seen)))
                  (multiple-value-bind (acc stop)
                      (funcall function acc current-node)
                    (when stop
                      (return-from fold-graph acc))
                    (reduce
                     (lambda (acc subnode)
                       (%fold seen acc subnode))
                     (funcall next-nodes current-node)
                     :initial-value acc)))))))
    (%fold nil initial-value node)))
