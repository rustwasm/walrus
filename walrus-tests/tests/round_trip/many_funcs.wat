(module
  (type (;0;) (func (result i32)))
  (func $a (type 0)
    i32.const 0)
  (func $b (type 0)
    (i32.add (i32.const 1) (i32.const 2)))
  (func $c (type 0)
    (i32.add
      (i32.const 3)
      (i32.add (i32.const 4) (i32.const 5))))
  (func $d (type 0)
    (i32.add
      (i32.add (i32.const 0) (i32.const 1))
      (i32.add (i32.const 2) (i32.const 3))))
  (export "a" (func $a))
  (export "b" (func $b))
  (export "c" (func $c))
  (export "d" (func $d)))

;; Note that these functions get properly sorted from largest to smallest.

;; CHECK: (module
;; NEXT:    (type (;0;) (func (result i32)))
;; NEXT:    (func $d (type 0) (result i32)
;; NEXT:      i32.const 0
;; NEXT:      i32.const 1
;; NEXT:      i32.add
;; NEXT:      i32.const 2
;; NEXT:      i32.const 3
;; NEXT:      i32.add
;; NEXT:      i32.add)
;; NEXT:    (func $c (type 0) (result i32)
;; NEXT:      i32.const 3
;; NEXT:      i32.const 4
;; NEXT:      i32.const 5
;; NEXT:      i32.add
;; NEXT:      i32.add)
;; NEXT:    (func $b (type 0) (result i32)
;; NEXT:      i32.const 1
;; NEXT:      i32.const 2
;; NEXT:      i32.add)
;; NEXT:    (func $a (type 0) (result i32)
;; NEXT:      i32.const 0)
;; NEXT:    (export "a" (func $a))
;; NEXT:    (export "b" (func $b))
;; NEXT:    (export "c" (func $c))
;; NEXT:    (export "d" (func $d)))

