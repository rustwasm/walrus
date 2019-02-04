(module
  (table 1 anyfunc)
  (func)
  (elem (i32.const 1) 0)
  (elem (i32.const 2) 0)
  (export "foo" (table 0))
  )

;; CHECK: (elem (;0;) (i32.const 1) 0 0)
