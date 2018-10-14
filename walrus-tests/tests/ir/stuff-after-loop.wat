(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0)
    loop
    end
    i32.const 1)
  (export "inf_loop_with_return" (func 0)))

;; CHECK: func {
;; NEXT:    ;; function exit
;; NEXT:    block_0(i32):
;; NEXT:      (return ((i32.const 1)))
;; NEXT:  
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (br block_3 ())
;; NEXT:  
;; NEXT:    ;; post-loop continuation block
;; NEXT:    block_2():
;; NEXT:      (br block_0 ((i32.const 1)))
;; NEXT:  
;; NEXT:    ;; loop
;; NEXT:    block_3():
;; NEXT:      (br block_3 ())
;; NEXT:  }
