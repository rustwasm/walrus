(module
  (import "x" "y" (global i32))
  (global i32 (global.get 0))
  (export "x" (global 1)))

;; CHECK: (module
;; NEXT:    (import "x" "y" (global (;0;) i32))
;; NEXT:    (global (;1;) i32 (global.get 0))
;; NEXT:    (export "x" (global 1)))

