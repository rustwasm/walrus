(module
  (type (func))

  (table anyfunc (elem 1))

  (func
    i32.const 0
    call_indirect
    )

  (func)

  (export "foo" (func 0))
  )

;; CHECK: (module
;; NEXT:    (type (;0;) (func))
;; NEXT:    (func (;0;) (type 0)
;; NEXT:      i32.const 0
;; NEXT:      call_indirect (type 0))
;; NEXT:    (func (;1;) (type 0))
;; NEXT:    (table (;0;) 1 1 anyfunc)
;; NEXT:    (export "foo" (func 0))
;; NEXT:    (elem (;0;) (i32.const 0) 1))

