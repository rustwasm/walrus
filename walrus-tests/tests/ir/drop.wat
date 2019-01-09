(module
  (type (;0;) (func))
  (func (type 0)
    i32.const 42
    drop))

;; CHECK: (func
;; NEXT:    (block ;; e0
;; NEXT:      (drop
;; NEXT:        (i32.const 42)
;; NEXT:      )
;; NEXT:    )
;; NEXT:  )
