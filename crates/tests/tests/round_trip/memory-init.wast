(module
  (import "x" "y" (global i32))
  (memory 1)
  (func)
  (data (global.get 0) "")
  (export "x" (memory 0)))

;; CHECK: (module
;; NEXT:    (import "x" "y" (global (;0;) i32))
;; NEXT:    (memory (;0;) 1)
;; NEXT:    (export "x" (memory 0))
;; NEXT:    (data (;0;) (global.get 0) ""))

