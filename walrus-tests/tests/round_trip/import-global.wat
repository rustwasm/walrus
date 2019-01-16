(module
  (import "" "" (global i32))
  (export "b" (global 0))
  )

;; CHECK: (module
;; NEXT:    (import "" "" (global (;0;) i32))
;; NEXT:    (export "b" (global 0)))
