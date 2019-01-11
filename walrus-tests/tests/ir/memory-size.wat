(module
  (memory 0)
  (func (;0;) (result i32)
    memory.size )
  (export "get" (func 0)))

;; CHECK: (func
;; NEXT:    (block ;; e0
;; NEXT:      (memory.size 0)
;; NEXT:    )
;; NEXT:  )

