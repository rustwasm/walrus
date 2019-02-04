;; Non-tree-like, stack-y expressions.

(module
  (import "env" "blackbox" (func $blackbox (param i32) (result i32)))
  (func (export "$f") (result i32)
    (call $blackbox (i32.const 1))
    (drop (call $blackbox (i32.const 2)))
    (call $blackbox (i32.const 3))
    i32.add
  ))

;; CHECK: (func
;; NEXT:     (block
;; NEXT:       (I32Add
;; NEXT:         (with.side.effects
;; NEXT:           (call 0
;; NEXT:             (const 1)
;; NEXT:           )
;; NEXT:           (drop
;; NEXT:             (call 0
;; NEXT:               (const 2)
;; NEXT:             )
;; NEXT:           )
;; NEXT:         )
;; NEXT:         (call 0
;; NEXT:           (const 3)
;; NEXT:         )
;; NEXT:       )
;; NEXT:     )
;; NEXT:   )

