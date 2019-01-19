(module
  (type (;0;) (func (param i32) (result i32)))
  (func $do_select (type 0) (param i32) (result i32)
    i32.const 2
    i32.const 1
    local.get 0
    select)
  (export "do_select" (func $do_select)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (param i32) (result i32)))
;; NEXT:    (func $do_select (type 0) (param i32) (result i32)
;; NEXT:      i32.const 2
;; NEXT:      i32.const 1
;; NEXT:      local.get 0
;; NEXT:      select)
;; NEXT:    (export "do_select" (func $do_select)))
