;; Non-tree-like, stack-y expressions.

(module
  (import "env" "blackbox" (func $blackbox (param i32) (result i32)))
  (func (export "$f") (result i32)
    (call $blackbox (i32.const 1))
    (drop (call $blackbox (i32.const 2)))
    (call $blackbox (i32.const 3))
    i32.add
  ))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (param i32) (result i32)))
;; NEXT:    (type (;1;) (func (result i32)))
;; NEXT:    (import "env" "blackbox" (func $blackbox (type 0)))
;; NEXT:    (func (;1;) (type 1) (result i32)
;; NEXT:      i32.const 1
;; NEXT:      call $blackbox
;; NEXT:      i32.const 2
;; NEXT:      call $blackbox
;; NEXT:      drop
;; NEXT:      i32.const 3
;; NEXT:      call $blackbox
;; NEXT:      i32.add)
;; NEXT:    (export "$f" (func 1)))


