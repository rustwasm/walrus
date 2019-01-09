;; Calls with parameters.

(module
  (type (;0;) (func (param i32) (result i32)))
  (import "env" "f" (func $f (type 0)))
  (func $g (type 0) (param i32) (result i32)
    (get_local 0)
    (call $f))
  (export "g" (func $g)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (param i32) (result i32)))
;; NEXT:    (import "env" "f" (func (;0;) (type 0)))
;; NEXT:    (func (;1;) (type 0) (param i32) (result i32)
;; NEXT:      (local i32)
;; NEXT:      get_local 0
;; NEXT:      call 0)
;; NEXT:    (export "g" (func 1)))
