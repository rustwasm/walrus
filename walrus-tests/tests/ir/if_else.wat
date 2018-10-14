(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0)
    get_local 0
    if (result i32)
      i32.const 1
    else
      i32.const 2
    end)
  (export "if_else" (func 0)))

;; CHECK: func {
;; NEXT:    ;; function exit
;; NEXT:    block_0(i32):
;; NEXT:      (return ((phi)))
;; NEXT:  
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (if/else (get_local 0) block_3 block_4)
;; NEXT:  
;; NEXT:    ;; if/else continuation
;; NEXT:    block_2(i32):
;; NEXT:      (br block_0 ((phi)))
;; NEXT:  
;; NEXT:    ;; consequent
;; NEXT:    block_3(i32):
;; NEXT:      (br block_2 ((i32.const 1)))
;; NEXT:  
;; NEXT:    ;; alternative
;; NEXT:    block_4(i32):
;; NEXT:      (br block_2 ((i32.const 2)))
;; NEXT:  }
