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
  "Return a list of all types in the graph"
  (remove-duplicates
   (fold-graph
    type-node (sera:flip #'cons) nil #'type-node-direct-subtypes)))

(sera:-> find-type-node (symbol type-node)
         (values (or null type-node) &optional))
(defun find-type-node (name type-node)
  "Find a type node T which has the name NAME and T ⊂ TYPE-NODE"
  (flet ((fn (acc node)
           (declare (ignore acc))
           (if (eql (type-node-name node) name)
               (values node t)
               (values nil  nil))))
    (fold-graph type-node #'fn nil #'type-node-direct-subtypes)))

(sera:-> check-type-system (type-node)
         (values type-node &optional))
(defun check-type-system (type-node)
  "Return TYPE-NODE if a type system for which it is a top node is
defined correctly, otherwise signal TYPESYSTEM-ERROR."
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
  "Return the join of T1 and T2. Join T is the minimal type for which
T1 ⊂ T and T2 ⊂ T. TOP is the top type for the type system."
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
  "Return the meet of T1 and T2. Meet T is the maximal type for which
T ⊂ T1 and T ⊂ T2. TOP is the top type for the type system."
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
(deftype type-node-order () '(member :eq :lt :gt nil))

(sera:-> type-node-order (type-node type-node type-node)
         (values type-node-order &optional))
(defun type-node-order (top t1 t2)
  "Return :EQ when T1 = T2, :LT when T1 < T2, :GT when T1 > T2 or NIL
otherwise. TOP is the top type of the type system."
  (if (eq t1 t2) :eq
      (let ((meet (meet top t1 t2)))
        (cond
          ((eq meet t1) :lt)
          ((eq meet t2) :gt)))))

(sera:-> ge (type-node-order)
         (values boolean &optional))
(defun ge (order)
  "Return T if ORDER is either :EQ or :GT."
  (sera:true
   (member order '(:gt :eq))))

(sera:-> le (type-node-order)
         (values boolean &optional))
(defun le (order)
  "Return T if ORDER is either :EQ or :LT."
  (sera:true
   (member order '(:lt :eq))))

(sera:-> types-intersect-p (type-node type-node type-node)
         (values boolean &optional))
(defun types-intersect-p (top t1 t2)
  "Return T if T1 ∧ T2 is not the bottom type."
  (not (eq (find-type-node nil top)
           (meet top t1 t2))))
