(module
  (type (;0;) (func (result i32)))
  (func $f (type 0) (result i32)
    unreachable
    i32.const 42)
  (export "f" (func $f)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (result i32)))
;; NEXT:    (func $f (type 0) (result i32)
;; NEXT:      unreachable)
;; NEXT:    (export "f" (func $f)))
