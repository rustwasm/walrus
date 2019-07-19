(module
  (memory 1)

  (func
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

  (data "A")
  (data (i32.const 0) "b")
  (data "C")
)

;; CHECK: (func
;; NEXT:    (block
;; NEXT:      (memory.init 0 0
;; NEXT:        (const 1)
;; NEXT:        (const 2)
;; NEXT:        (const 3)
;; NEXT:      )
;; NEXT:      (data.drop 2)
;; NEXT:      (memory.copy 0 0
;; NEXT:        (const 1)
;; NEXT:        (const 2)
;; NEXT:        (const 3)
;; NEXT:      )
;; NEXT:      (memory.fill 0
;; NEXT:        (const 1)
;; NEXT:        (const 2)
;; NEXT:        (const 3)
;; NEXT:      )
;; NEXT:    )
;; NEXT:  )
