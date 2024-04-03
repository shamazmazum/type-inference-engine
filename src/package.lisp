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
   #:unknown-literal
   #:unknown-variable
   #:fndb-entry-exists
   #:malformed-defun
   #:typecheck-error

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
   #:types-intersect-p
   #:print-graphviz-representation

   ;; Defknown facility
   #:defknown
   #:defknown*
   #:definitializer
   #:make-fndb
   #:known-functions-equivalent-p

   ;; Initializer pseudo-functions
   #:initialize
   #:literal-initializer

   ;; Control flow & inference across statements
   #:var->type
   #:vars->types
   #:parse-code
   #:statement
   #:statement-assigns-to
   #:statement-function
   #:statement-arguments
   #:flat-control-node
   #:flat-control-node-from
   #:flat-control-node-to
   #:flat-control-node-statements
   #:forward-inference-rule
   #:backward-inference-rule

   ;; Global inference
   #:infer-types

   ;; Side-effects of compilation
   #:compile-function

   ;; Restarts
   #:fndb-abort
   #:fndb-replace))
