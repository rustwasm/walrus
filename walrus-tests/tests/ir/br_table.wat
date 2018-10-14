(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0)
    block
      block
        block
          get_local 0
          br_table 0 1 2
        end
        i32.const 300
        return
      end
      i32.const 200
      return
    end
    i32.const 100))

;; CHECK: func {
;; NEXT:    ;; function entry
;; NEXT:    block_1():
;; NEXT:      (br block_3 ())
;; NEXT:  
;; NEXT:    ;; block
;; NEXT:    block_3():
;; NEXT:      (br block_5 ())
;; NEXT:  
;; NEXT:    ;; block
;; NEXT:    block_5():
;; NEXT:      (br block_7 ())
;; NEXT:  
;; NEXT:    ;; block
;; NEXT:    block_7():
;; NEXT:      (br_table (get_local 0) [block_6 block_4] block_2 ())
;; NEXT:  
;; NEXT:    ;; block continuation
;; NEXT:    block_2():
;; NEXT:      (br block_0 ((i32.const 100)))
;; NEXT:  
;; NEXT:    ;; function exit
;; NEXT:    block_0(i32):
;; NEXT:      (return ((i32.const 100)))
;; NEXT:  
;; NEXT:    ;; block continuation
;; NEXT:    block_6():
;; NEXT:      (return ((i32.const 300)))
;; NEXT:  
;; NEXT:    ;; block continuation
;; NEXT:    block_4():
;; NEXT:      (return ((i32.const 200)))
;; NEXT:  }
