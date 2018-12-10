(module
  (type (;0;) (func))
  (func $f (type 0)
    (drop (i32.const 42)))
  (export "f" (func $f)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func))
;; NEXT:    (func (;0;) (type 0)
;; NEXT:      i32.const 42
;; NEXT:      drop)
;; NEXT:    (export "f" (func 0)))
