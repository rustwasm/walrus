(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0)
    local.get 0
    if (result i32)
      i32.const 1
    else
      i32.const 2
    end)
  (export "if_else" (func 0)))

;; CHECK: (func
;; NEXT:    (block ;; e0
;; NEXT:      (if
;; NEXT:        (local.get 0)
;; NEXT:        (block ;; e2
;; NEXT:          (i32.const 1)
;; NEXT:        )
;; NEXT:        (block ;; e4
;; NEXT:          (i32.const 2)
;; NEXT:        )
;; NEXT:      )
;; NEXT:    )
;; NEXT:  )
