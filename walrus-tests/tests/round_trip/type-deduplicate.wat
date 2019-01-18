(module
  (type (;0;) (func))
  (type (;1;) (func))
  (func (;0;) (type 0))
  (func (;1;) (type 1))

  (export "a" (func 0))
  (export "b" (func 1))
  )

;; CHECK: (module
;; NEXT:    (type
;; NEXT:    (func
;; NEXT:    (func
;; NEXT:    (export
;; NEXT:    (export
