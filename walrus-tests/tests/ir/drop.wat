(module
  (type (;0;) (func))
  (func (type 0)
    i32.const 42
    drop))

;; CHECK: func {
;; NEXT:    ;; function exit
;; NEXT:    block_0():
;; NEXT:      (return ())
;; NEXT:  
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (drop (i32.const 42))
;; NEXT:      (br block_0 ())
;; NEXT:  }
