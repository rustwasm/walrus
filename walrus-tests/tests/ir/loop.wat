(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    loop
    end)
  (export "inf_loop" (func 0)))

;; CHECK: (func
;; NEXT:    (block ;; e0 (function entry)
;; NEXT:      (loop ;; e1 (loop)
;; NEXT:      )
;; NEXT:    )
;; NEXT:  )
