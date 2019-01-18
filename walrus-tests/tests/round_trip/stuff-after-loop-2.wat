(module
  (type (;0;) (func (result i32)))
  (func $f (type 0)
    loop
      br 0
    end
    i32.const 1)
  (export "f" (func $f)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (result i32)))
;; NEXT:    (func $f (type 0) (result i32)
;; NEXT:      loop  ;; label = @1
;; NEXT:        br 0 (;@1;)
;; NEXT:      end
;; NEXT:      i32.const 1)
;; NEXT:    (export "f" (func $f)))
