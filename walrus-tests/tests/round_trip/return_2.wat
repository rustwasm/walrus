(module
  (func $f (result i32)
    (return (i32.const 1))
    i32.const 2)
  (export "f" (func $f)))

;; CHECK: (func (;0;) (type 0) (result i32)
;; NEXT:    i32.const 1
;; NEXT:    return)
