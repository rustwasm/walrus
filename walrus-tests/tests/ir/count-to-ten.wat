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

;; CHECK: func {
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (set_local 0 (i32.const 9))
;; NEXT:      (br block_3 ())
;; NEXT:  
;; NEXT:    ;; loop
;; NEXT:    block_3():
;; NEXT:      (br_if (i32.eqz (get_local 0)) block_2 ())
;; NEXT:      (set_local 0 (i32.add (get_local 0) (i32.const 1)))
;; NEXT:      (br block_3 ())
;; NEXT:  
;; NEXT:    ;; post-loop continuation block
;; NEXT:    block_2():
;; NEXT:      (br block_0 ((i32.const 10)))
;; NEXT:  
;; NEXT:    ;; function exit
;; NEXT:    block_0(i32):
;; NEXT:      (return ((i32.const 10)))
;; NEXT:  }
