;; Instructions after an unreachable should not be emitted, but unreachable
;; instructions outside the block should be.

(module
  (type (;0;) (func (result i32)))
  (func $f (type 0) (result i32)
    block
      unreachable
    end
    i32.const 42)
  (export "f" (func $f)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (result i32)))
;; NEXT:    (func $f (type 0) (result i32)
;; NEXT:      block  ;; label = @1
;; NEXT:        unreachable
;; NEXT:      end
;; NEXT:      i32.const 42)
;; NEXT:    (export "f" (func $f)))
