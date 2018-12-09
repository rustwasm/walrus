(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0)
    loop
      br 0
    end
    i32.const 1))

;; CHECK: (func
;; NEXT:    (block ;; e0 (function entry)
;; NEXT:      (loop ;; e1 (loop)
;; NEXT:        (br
;; NEXT:          e0 ;; block
;; NEXT:          ()
;; NEXT:        )
;; NEXT:      )
;; NEXT:      (i32.const 1)
;; NEXT:    )
;; NEXT:  )
