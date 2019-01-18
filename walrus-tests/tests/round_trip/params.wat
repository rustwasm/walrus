(module
  (func (param i32 i32) (result i32)
    local.get 1
    local.get 0
    i32.add)
  (export "foo" (func 0)))

;; CHECK: (func (;0;) (type 0) (param i32 i32) (result i32)
;; NEXT:    local.get 1
;; NEXT:    local.get 0
;; NEXT:    i32.add)
