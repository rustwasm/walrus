(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0)
    block
      block
        block
          local.get 0
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
;; NEXT:    (block
;; NEXT:      (block
;; NEXT:        (block
;; NEXT:          (block
;; NEXT:            (br.table (;default:e1  [e3 e2];)
;; NEXT:              (local.get 0)
;; NEXT:            )
;; NEXT:          )
;; NEXT:          (return
;; NEXT:            (const 300)
;; NEXT:          )
;; NEXT:        )
;; NEXT:        (return
;; NEXT:          (const 200)
;; NEXT:        )
;; NEXT:      )
;; NEXT:      (const 100)
;; NEXT:    )
;; NEXT:  )

