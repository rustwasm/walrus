(module
  (type (;0;) (func (param i32) (result i32)))
  (func $inc (type 0) (param i32) (result i32)
    get_local 0
    i32.const 1
    i32.add))

;; CHECK: (func
;; NEXT:    (block ;; e0 (function entry)
;; NEXT:      (i32.add
;; NEXT:        (get_local 0)
;; NEXT:        (i32.const 1)
;; NEXT:      )
;; NEXT:    )
;; NEXT:  )
