(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (local i32)
    block
      get_local 0
      set_local 1
      loop
        ;; if local 0 == 0, break
        get_local 0
        i32.eqz
        br_if 1

        ;; local 1 = local 0 * local 1
        get_local 1
        get_local 0
        i32.mul
        set_local 1

        ;; local 0 = local 0 - 1
        get_local 0
        i32.const 1
        i32.sub
        set_local 0
      end
    end

    ;; return the accumulated value
    get_local 1)
  (export "fac" (func 0)))

;; CHECK: (func
;; NEXT:    (block ;; e0 (function entry)
;; NEXT:      (block ;; e1 (block)
;; NEXT:        (set_local
;; NEXT:          1
;; NEXT:          (get_local 0)
;; NEXT:        )
;; NEXT:        (loop ;; e4 (loop)
;; NEXT:          (br_if
;; NEXT:            e0
;; NEXT:            (i32.eqz
;; NEXT:              (get_local 0)
;; NEXT:            )
;; NEXT:            ()
;; NEXT:          )
;; NEXT:          (set_local
;; NEXT:            1
;; NEXT:            (i32.mul
;; NEXT:              (get_local 1)
;; NEXT:              (get_local 0)
;; NEXT:            )
;; NEXT:          )
;; NEXT:          (set_local
;; NEXT:            0
;; NEXT:            (i32.sub
;; NEXT:              (get_local 0)
;; NEXT:              (i32.const 1)
;; NEXT:            )
;; NEXT:          )
;; NEXT:        )
;; NEXT:      )
;; NEXT:      (get_local 1)
;; NEXT:    )
;; NEXT:  )
