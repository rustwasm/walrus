(module
  (func $f (result i32)
    (return (i32.const 1)))
  (export "f" (func $f)))

;; CHECK: (func $f (type 0) (result i32)
;; NEXT:    i32.const 1
;; NEXT:    return)
