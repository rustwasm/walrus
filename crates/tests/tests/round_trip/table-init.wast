(module
  (import "x" "y" (global i32))
  (table 1 anyfunc)
  (func)
  (elem (global.get 0) 0)
  (export "x" (table 0)))

;; CHECK: (module
;; NEXT:    (type
;; NEXT:    (import "x" "y" (global (;0;) i32))
;; NEXT:    (func
;; NEXT:    (table
;; NEXT:    (export "x" (table 0))
;; NEXT:    (elem (;0;) (global.get 0) 0))
