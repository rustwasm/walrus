;; Do not gc used globals.

(module
  (global $used i32 (i32.const 666))
  (export "g" (global $used)))

;; CHECK: (module
;; NEXT:    (global (;0;) i32 (i32.const 666))
;; NEXT:    (export "g" (global 0)))

