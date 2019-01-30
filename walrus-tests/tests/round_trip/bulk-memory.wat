(module
  (memory 1)

  (func (export "a")
    (memory.init 0
      (i32.const 1)
      (i32.const 2)
      (i32.const 3))
    (data.drop 2)

    (memory.copy
      (i32.const 1)
      (i32.const 2)
      (i32.const 3))

    (memory.fill
      (i32.const 1)
      (i32.const 2)
      (i32.const 3))
  )

  (data passive "A")
  (data (i32.const 0) "b")
  (data passive "C")
)

;; CHECK: (module
;; NEXT:    (type (;0;) (func))
;; NEXT:    (func (;0;) (type 0)
;; NEXT:      i32.const 1
;; NEXT:      i32.const 2
;; NEXT:      i32.const 3
;; NEXT:      memory.init 1
;; NEXT:      data.drop 2
;; NEXT:      i32.const 1
;; NEXT:      i32.const 2
;; NEXT:      i32.const 3
;; NEXT:      memory.copy
;; NEXT:      i32.const 1
;; NEXT:      i32.const 2
;; NEXT:      i32.const 3
;; NEXT:      memory.fill)
;; NEXT:    (memory (;0;) 1)
;; NEXT:    (export "a" (func 0))
;; NEXT:    (data (;0;) (i32.const 0) "b")
;; NEXT:    (data (;1;) passive "A")
;; NEXT:    (data (;2;) passive "C"))
