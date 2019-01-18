(module
  (type (;0;) (func (param i32) (result i32)))
  (func $if_else (type 0)
    local.get 0
    if (result i32)
      i32.const 1
    else
      i32.const 2
    end)
  (export "if_else" (func $if_else)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (param i32) (result i32)))
;; NEXT:    (func $if_else (type 0) (param i32) (result i32)
;; NEXT:      local.get 0
;; NEXT:      if (result i32)  ;; label = @1
;; NEXT:        i32.const 1
;; NEXT:      else
;; NEXT:        i32.const 2
;; NEXT:      end)
;; NEXT:    (export "if_else" (func $if_else)))
