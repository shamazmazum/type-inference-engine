(defpackage type-inference-engine
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:si   #:stateless-iterators)
                    (#:alex #:alexandria))
  (:export
   ;; Conditions
   #:inference-error
   #:cycle-detected
   #:typesystem-error
   #:incorrect-definition
   #:arity-error
   #:unknown-function
   #:fndb-entry-exists

   ;; Graphs
   #:fold-graph

   ;; Type system
   #:type-node
   #:type-node-name
   #:type-node-documentation
   #:type-node-direct-subtypes
   #:type-node-order
   #:ge #:le

   #:find-type-node
   #:flatten-type-graph
   #:check-type-system
   #:join
   #:meet
   #:type-subtype-p
   #:types-intersect-p

   ;; Defknown facility
   #:statement
   #:forward-inference-rule
   #:backward-inference-rule
   #:defknown
   #:definitializer
   #:make-fndb

   ;; Control flow & inference across statements
   #:var->type
   #:vars->types
   #:parse-code
   #:flat-control-flow-graph

   ;; Global inference
   #:infer-types

   ;; Restarts
   #:fndb-abort
   #:fndb-replace))
