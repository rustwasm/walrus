(module
  (import "" "" (memory 1))
  (export "b" (memory 0))
  )

;; CHECK: (module
;; NEXT:    (import "" "" (memory (;0;) 1))
;; NEXT:    (export "b" (memory 0)))

