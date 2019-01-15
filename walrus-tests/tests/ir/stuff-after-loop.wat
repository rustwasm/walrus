(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0)
    loop
    end
    i32.const 1))

;; CHECK: (func
;; NEXT:    (block
;; NEXT:      (loop)
;; NEXT:      (const 1)
;; NEXT:    )
;; NEXT:  )
