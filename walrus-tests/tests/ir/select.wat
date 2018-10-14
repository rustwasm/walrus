(module
  (type (;0;) (func (param i32) (result i32)))
  (func $do_select (type 0) (param i32) (result i32)
    i32.const 2
    i32.const 1
    get_local 0
    select)
  (memory (;0;) 16)
  (export "do_select" (func $do_select)))

;; CHECK: func {
;; NEXT:    ;; function exit
;; NEXT:    block_0(i32):
;; NEXT:      (return ((select (get_local 0) (i32.const 1) (i32.const 2))))
;; NEXT:  
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (br block_0 ((select (get_local 0) (i32.const 1) (i32.const 2))))
;; NEXT:  }
