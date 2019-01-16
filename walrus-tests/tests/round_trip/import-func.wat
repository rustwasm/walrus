(module
  (type (func))
  (import "" "" (func (type 0)))
  (export "b" (func 0))
  )

;; CHECK: (module
;; NEXT:    (type (;0;) (func))
;; NEXT:    (import "" "" (func (;0;) (type 0)))
;; NEXT:    (export "b" (func 0)))
