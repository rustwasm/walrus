(module
  (type (;0;) (func (result i32)))
  (func $inc (type 0) (local i32)
    i32.const 0
    drop
    block
      i32.const 1
      drop
    end
    i32.const 2))

;; CHECK: func {
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (drop (i32.const 0))
;; NEXT:      (br block_3 ())
;; NEXT: 
;; NEXT:    ;; block
;; NEXT:    block_3():
;; NEXT:      (drop (i32.const 1))
;; NEXT:      (br block_2 ())
;; NEXT: 
;; NEXT:    ;; block continuation
;; NEXT:    block_2():
;; NEXT:      (br block_0 ((i32.const 2)))
;; NEXT: 
;; NEXT:    ;; function exit
;; NEXT:    block_0(i32):
;; NEXT:      (return ((i32.const 2)))
;; NEXT:  }
