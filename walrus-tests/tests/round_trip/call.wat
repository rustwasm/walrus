(module
  (type (;0;) (func (result i32)))
  (import "env" "f" (func $f (type 0)))
  (func $g (type 0) (result i32)
    (call $f))
  (export "g" (func $g)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (result i32)))
;; NEXT:    (import "env" "f" (func (;0;) (type 0)))
;; NEXT:    (func (;1;) (type 0) (result i32)
;; NEXT:      call 0)
;; NEXT:    (export "g" (func 1)))

