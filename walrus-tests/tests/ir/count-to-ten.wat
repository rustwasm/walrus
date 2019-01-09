(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (local i32)
    (set_local 0 (i32.const 9))
    loop
      (br_if 0 (i32.eqz (get_local 0)))

      (set_local 0 (i32.add (get_local 0) (i32.const 1)))
    end
    i32.const 10)
  (export "count_to_ten" (func 0)))

;; CHECK: (func
;; NEXT:    (block ;; e0
;; NEXT:      (set_local
;; NEXT:        0
;; NEXT:        (i32.const 9)
;; NEXT:      )
;; NEXT:      (loop ;; e3
;; NEXT:        (br_if
;; NEXT:          e0
;; NEXT:          (i32.eqz
;; NEXT:            (get_local 0)
;; NEXT:          )
;; NEXT:          ()
;; NEXT:        )
;; NEXT:        (set_local
;; NEXT:          0
;; NEXT:          (i32.add
;; NEXT:            (get_local 0)
;; NEXT:            (i32.const 1)
;; NEXT:          )
;; NEXT:        )
;; NEXT:      )
;; NEXT:      (i32.const 10)
;; NEXT:    )
;; NEXT:  )
