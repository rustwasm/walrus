(module
  (type (;0;) (func (result i32)))
  (func $f (type 0) (local i32)
    i32.const 0
    drop
    block
      i32.const 1
      drop
    end
    i32.const 2)
  (export "f" (func $f)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (result i32)))
;; NEXT:    (func (;0;) (type 0) (result i32)
;; NEXT:      i32.const 0
;; NEXT:      drop
;; NEXT:      block  ;; label = @1
;; NEXT:        i32.const 1
;; NEXT:        drop
;; NEXT:      end
;; NEXT:      i32.const 2)
;; NEXT:    (export "f" (func 0)))
