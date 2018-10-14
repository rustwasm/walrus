(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    loop
    end)
  (export "inf_loop" (func 0)))

;; CHECK: func {
;; NEXT:    ;; function exit
;; NEXT:    block_0():
;; NEXT:      (return ())
;; NEXT:  
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (br block_3 ())
;; NEXT:  
;; NEXT:    ;; post-loop continuation block
;; NEXT:    block_2():
;; NEXT:      (br block_0 ())
;; NEXT:  
;; NEXT:    ;; loop
;; NEXT:    block_3():
;; NEXT:      (br block_3 ())
;; NEXT:  }
