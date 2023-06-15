(in-package :type-inference-engine)

(sera:defconstructor type-node
  (name            symbol)
  (documentation   string)
  (direct-subtypes list))

(defmethod print-object ((type-node type-node) stream)
  (print-unreadable-object (type-node stream :type t :identity t)
    (princ (type-node-name type-node) stream)))

;; TODO: What about some generic graph traversal?
(sera:-> paths-to-bottom (type-node)
         (values list &optional))
(defun paths-to-bottom (type-node)
  "Return a list of all paths from the TYPE-NODE to the bottom type."
  (let (paths)
    (labels ((%path (node acc)
               (let ((acc (cons node acc))
                     (subtypes (type-node-direct-subtypes node)))
                 (if subtypes
                     (dolist (subtype subtypes)
                       (%path subtype acc))
                     (push (reverse acc) paths)))))
      (%path type-node nil)
      paths)))

(sera:-> flatten-type-graph (type-node)
         (values list &optional))
(defun flatten-type-graph (type-node)
  "Return a list of all types in a type system with the top node
@c(type-node)."
  (remove-duplicates
   (fold-graph
    type-node (sera:flip #'cons) nil #'type-node-direct-subtypes)))

(sera:-> find-type-node (symbol type-node)
         (values (or null type-node) &optional))
(defun find-type-node (name type-node)
  "Find a type node @c(A) which has the name @c(name) and @c(A ⊂
type-node)."
  (flet ((fn (acc node)
           (declare (ignore acc))
           (if (eql (type-node-name node) name)
               (values node t)
               (values nil  nil))))
    (fold-graph type-node #'fn nil #'type-node-direct-subtypes)))

(sera:-> check-type-system (type-node)
         (values type-node &optional))
(defun check-type-system (type-node)
  "Return @c(type-node) if a type system for which this node is a top
node is defined correctly, otherwise signal @c(typesystem-error) or
just do not return control."
  ;; If the type system has cycles (which is wrong), PATHS-TO-BOTTOM
  ;; will never terminate
  (let* ((paths   (paths-to-bottom type-node))
         (tops    (mapcar #'car paths))
         (bottoms (mapcar (alex:compose #'car #'last) paths)))
    (unless
        (and
         ;; All paths start with the same node
         (every (sera:hook #'eq (constantly (car tops))) tops)
         ;; All paths end with the same node
         (every (sera:hook #'eq (constantly (car bottoms))) bottoms)
         ;; Current limitation: all bottom nodes must be called NIL
         (every (sera:fork #'eq #'type-node-name (constantly nil)) bottoms)
         ;; All paths do not have nodes with the same name
         (every (sera:fork #'= #'length
                           (alex:compose #'length
                                         (alex:rcurry #'remove-duplicates
                                                      :test #'eq
                                                      :key  #'type-node-name)))
                paths))
      (error 'typesystem-error :type-system type-node)))
  type-node)

;; FIXME: I do not remember if there is a function from standard
;; library for this.
(sera:-> find-common-head (list list)
         (values list &optional))
(defun find-common-head (l1 l2)
  "Find the longest common prefix for L1 and L2."
  (labels ((%common (l1 l2 acc)
             (if (or (null l1)
                     (null l2)
                     (not (eql (car l1)
                               (car l2))))
                 acc
                 (%common (cdr l1)
                          (cdr l2)
                          (cons (car l1) acc)))))
    (reverse
     (%common l1 l2 nil))))

(sera:-> join (type-node type-node type-node)
         (values type-node &optional))
(defun join (top t1 t2)
  "Return the join of types @c(t1) and @c(t2). Join @c(t) is the
minimal type for which @c(t1 ⊂ t) and @c(t2 ⊂ t). @c(Top) is the top
type for the type system."
  (flet ((path-to-type (type)
           (remove
            nil (mapcar
                 (alex:compose
                  #'reverse
                  (alex:curry #'member type)
                  #'reverse)
                 (paths-to-bottom top)))))
    (let* ((paths-to-t1 (path-to-type t1))
           (paths-to-t2 (path-to-type t2))
           (common-paths (loop for path1 in paths-to-t1 append
                               (loop for path2 in paths-to-t2 collect
                                     (find-common-head path1 path2))))
           (lengths (mapcar #'length common-paths))
           (max-length (reduce #'max lengths)))
      (car (last (find max-length common-paths :key #'length))))))

(sera:-> meet (type-node type-node type-node)
         (values type-node &optional))
(defun meet (top t1 t2)
  "Return the meet of types @c(t1) and @c(t2). Meet @c(t) is the
maximal type for which @c(t ⊂ t1) and @c(t ⊂ t2). @c(Top) is the top
type for the type system."
  (declare (ignore top))
  (let* ((paths-from-t1 (paths-to-bottom t1))
         (paths-from-t2 (paths-to-bottom t2))
         (common-paths (loop for path1 in paths-from-t1 append
                             (loop for path2 in paths-from-t2 collect
                                   (find-common-head (reverse path1) (reverse path2)))))
         (lengths (mapcar #'length common-paths))
         (max-length (reduce #'max lengths)))
    (car (last (find max-length common-paths :key #'length)))))

;; Relation between types: equal, greater, less, not defined
(deftype type-node-order ()
  "An order between two types. Can be @c(:EQ), @c(:LT), @c(:GT) or
@c(NIL). The latter is to indicate that there is no order between two
types."
  '(member :eq :lt :gt nil))

(sera:-> type-node-order (type-node type-node type-node)
         (values type-node-order &optional))
(defun type-node-order (top t1 t2)
  "Return @c(:eq) when @c(t1 = t2), @c(:lt) when @c(t1 < t2), @c(:gt)
when @c(t1 > t2) or @c(nil) otherwise. @c(Top) is the top type of the
type system."
  (if (eq t1 t2) :eq
      (let ((meet (meet top t1 t2)))
        (cond
          ((eq meet t1) :lt)
          ((eq meet t2) :gt)))))

(sera:-> ge (type-node-order)
         (values boolean &optional))
(defun ge (order)
  "Return @c(t) if @c(order) is either @c(:eq) or @c(:gt), @c(nil)
otherwise."
  (sera:true
   (member order '(:gt :eq))))

(sera:-> le (type-node-order)
         (values boolean &optional))
(defun le (order)
  "Return @c(t) if @c(order) is either @c(:eq) or @c(:lt), @c(nil)
otherwise."
  (sera:true
   (member order '(:lt :eq))))

(sera:-> types-intersect-p (type-node type-node type-node)
         (values boolean &optional))
(defun types-intersect-p (top t1 t2)
  "Return @c(t) if @c(t1 ∧ t2) is not the bottom type, @c(nil)
otherwise. @c(Top) is the top type of the type system."
  (not (eq (find-type-node nil top)
           (meet top t1 t2))))

(sera:-> print-graphviz-representation (type-node &optional stream)
         (values &optional))
(defun print-graphviz-representation (top &optional (stream *standard-output*))
  "Print representation of a type system with the top node @c(top) in
Graphviz format to the output stream @c(stream)."
  (flet ((collect-edges (node)
           (remove-duplicates
            (fold-graph
             node
             (lambda (acc node)
               (append acc
                       (mapcar
                        (alex:curry #'cons node)
                        (type-node-direct-subtypes node))))
             nil #'type-node-direct-subtypes)
            :test #'equal)))
    (format stream "digraph type_system {~%")
    (loop for pair in (collect-edges top) do
          (format stream "~a -> ~a;~%"
                  (type-node-name (car pair))
                  (type-node-name (cdr pair))))
    (format stream "}~%"))
  (values))
