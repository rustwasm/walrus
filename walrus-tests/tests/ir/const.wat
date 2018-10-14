(module
  (type (;0;) (func (result i32)))
  (func $inc (type 0) (result i32)
    i32.const 42)
  (table (;0;) 1 1 anyfunc)
  (memory (;0;) 16))

;; CHECK: func {
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (br block_0 ((i32.const 42)))
;; NEXT:  
;; NEXT:    ;; function exit
;; NEXT:    block_0(i32):
;; NEXT:      (return ((i32.const 42)))
;; NEXT:  }
