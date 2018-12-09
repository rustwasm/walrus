(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0)
    block
      block
        block
          get_local 0
          br_table 0 1 2
        end
        i32.const 300
        return
      end
      i32.const 200
      return
    end
    i32.const 100))

;; CHECK: (func
;; NEXT:    (block ;; e0 (function entry)
;; NEXT:      (block ;; e1 (block)
;; NEXT:        (block ;; e2 (block)
;; NEXT:          (block ;; e3 (block)
;; NEXT:            (br_table
;; NEXT:              (get_local 0)
;; NEXT:              e0 ;; default
;; NEXT:              [e2 e1]
;; NEXT:              ()
;; NEXT:            )
;; NEXT:          )
;; NEXT:          (return
;; NEXT:            (i32.const 300)
;; NEXT:          )
;; NEXT:        )
;; NEXT:        (return
;; NEXT:          (i32.const 200)
;; NEXT:        )
;; NEXT:      )
;; NEXT:      (i32.const 100)
;; NEXT:    )
;; NEXT:  )

