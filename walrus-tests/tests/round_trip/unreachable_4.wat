(module
  (func $f (result i32)
    unreachable
    i32.add)
  (export "f" (func $f)))

;; CHECK: (func (;0;) (type 0) (result i32)
;; NEXT:    unreachable)
