(module
  (global i32 (i32.const 0))
  (func $foo (local i32)
    global.get 0
    local.set 0)
  (export "foo" (func $foo))
  )

;; CHECK: (func $foo (type 0)
;; NEXT:    (local i32)
;; NEXT:    global.get 0
;; NEXT:    local.set 0)
