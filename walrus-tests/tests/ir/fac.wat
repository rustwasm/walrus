(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (local i32)
    block
      local.get 0
      local.set 1
      loop
        ;; if local 0 == 0, break
        local.get 0
        i32.eqz
        br_if 1

        ;; local 1 = local 0 * local 1
        local.get 1
        local.get 0
        i32.mul
        local.set 1

        ;; local 0 = local 0 - 1
        local.get 0
        i32.const 1
        i32.sub
        local.set 0
      end
    end

    ;; return the accumulated value
    local.get 1)
  (export "fac" (func 0)))

;; CHECK: (func
;; NEXT:    (block
;; NEXT:      (block
;; NEXT:        (local.set 1
;; NEXT:          (local.get 0)
;; NEXT:        )
;; NEXT:        (loop
;; NEXT:          (br.if (;e0;)
;; NEXT:            (I32Eqz
;; NEXT:              (local.get 0)
;; NEXT:            )
;; NEXT:          )
;; NEXT:          (local.set 1
;; NEXT:            (I32Mul
;; NEXT:              (local.get 1)
;; NEXT:              (local.get 0)
;; NEXT:            )
;; NEXT:          )
;; NEXT:          (local.set 0
;; NEXT:            (I32Sub
;; NEXT:              (local.get 0)
;; NEXT:              (const 1)
;; NEXT:            )
;; NEXT:          )
;; NEXT:        )
;; NEXT:      )
;; NEXT:      (local.get 1)
;; NEXT:    )
;; NEXT:  )
