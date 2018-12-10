(module
  (type (;0;) (func (param i32) (result i32)))
  (func $inc (type 0) (param i32) (result i32)
    (i32.add
      (get_local 0)
      (i32.const 1)))
  (export "inc" (func $inc)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (param i32) (result i32)))
;; NEXT:    (func (;0;) (type 0) (param i32) (result i32)
;; NEXT:      (local i32)
;; NEXT:      get_local 0
;; NEXT:      i32.const 1
;; NEXT:      i32.add)
;; NEXT:    (export "inc" (func 0)))
