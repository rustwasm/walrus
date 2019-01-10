(module
  (type (;0;) (func (param i32) (result i32)))
  (func $f (type 0)
    block
      block
        block
          local.get 0
          br_table 0 1 2
        end
        i32.const 300
        return
      end
      i32.const 200
      return
    end
    i32.const 100)
  (export "f" (func $f)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (param i32) (result i32)))
;; NEXT:    (func (;0;) (type 0) (param i32) (result i32)
;; NEXT:      (local i32)
;; NEXT:      block  ;; label = @1
;; NEXT:        block  ;; label = @2
;; NEXT:          block  ;; label = @3
;; NEXT:            local.get 0
;; NEXT:            br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
;; NEXT:          end
;; NEXT:          i32.const 300
;; NEXT:          return
;; NEXT:        end
;; NEXT:        i32.const 200
;; NEXT:        return
;; NEXT:      end
;; NEXT:      i32.const 100)
;; NEXT:    (export "f" (func 0)))
