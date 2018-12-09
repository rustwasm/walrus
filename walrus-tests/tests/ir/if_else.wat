(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0)
    get_local 0
    if (result i32)
      i32.const 1
    else
      i32.const 2
    end)
  (export "if_else" (func 0)))

;; CHECK: (func
;; NEXT:    (block ;; e0 (function entry)
;; NEXT:      (if
;; NEXT:        (get_local 0)
;; NEXT:        (block ;; e2 (consequent)
;; NEXT:          (i32.const 1)
;; NEXT:        )
;; NEXT:        (block ;; e4 (alternative)
;; NEXT:          (i32.const 2)
;; NEXT:        )
;; NEXT:      )
;; NEXT:    )
;; NEXT:  )
