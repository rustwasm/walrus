(module
  (type (;0;) (func (param i32) (result i32)))
  (func $inc (type 0) (param i32) (result i32)
    get_local 0
    i32.const 1
    i32.add)
  (table (;0;) 1 1 anyfunc)
  (memory (;0;) 16))

;; CHECK: func {
;; NEXT:    ;; function exit
;; NEXT:    block_0(i32):
;; NEXT:      (return ((i32.add (get_local 0) (i32.const 1))))
;; NEXT:  
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (br block_0 ((i32.add (get_local 0) (i32.const 1))))
;; NEXT:  }
