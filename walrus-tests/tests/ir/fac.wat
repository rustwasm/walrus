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

;; CHECK: func {
;; NEXT:    ;; function exit
;; NEXT:    block_0(i32):
;; NEXT:      (return ((get_local 1)))
;; NEXT:  
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (br block_3 ())
;; NEXT:  
;; NEXT:    ;; block continuation
;; NEXT:    block_2():
;; NEXT:      (br block_0 ((get_local 1)))
;; NEXT:  
;; NEXT:    ;; block
;; NEXT:    block_3():
;; NEXT:      (set_local 1 (get_local 0))
;; NEXT:      (br block_5 ())
;; NEXT:  
;; NEXT:    ;; post-loop continuation block
;; NEXT:    block_4():
;; NEXT:  
;; NEXT:    ;; loop
;; NEXT:    block_5():
;; NEXT:      (br_if (i32.eqz (get_local 0)) block_2 ())
;; NEXT:      (set_local 1 (i32.mul (get_local 1) (get_local 0)))
;; NEXT:      (set_local 0 (i32.sub (get_local 0) (i32.const 1)))
;; NEXT:      (br block_5 ())
;; NEXT:  }
