(module
  (type (;0;) (func (param i32) (result i32)))
  (func $inc (type 0) (param i32) (result i32)
    local.get 0
    i32.const 1
    i32.add))

;; CHECK: (func
;; NEXT:    (block
;; NEXT:      (I32Add
;; NEXT:        (local.get 0)
;; NEXT:        (const 1)
;; NEXT:      )
;; NEXT:    )
;; NEXT:  )
