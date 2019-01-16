(module
  (global i32 (i32.const 0))
  (func $foo
    (local i32 i32)
    local.get 0
    local.set 1)
  (export "foo" (func 0))
  )

;; CHECK: (func
;; NEXT:    (local i32 i32)
;; NEXT:    local.get 1
;; NEXT:    local.set 0)
