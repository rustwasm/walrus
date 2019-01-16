(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0)
    loop
      br 0
    end
    i32.const 1))

;; CHECK: (func
;; NEXT:    (block
;; NEXT:      (loop
;; NEXT:        (br (;e0;))
;; NEXT:      )
;; NEXT:      (const 1)
;; NEXT:    )
;; NEXT:  )
