(module
  (import "" "" (table 1 anyfunc))
  (export "b" (table 0))
  )

;; CHECK: (module
;; NEXT:    (import "" "" (table (;0;) 1 funcref))
;; NEXT:    (export "b" (table 0)))
