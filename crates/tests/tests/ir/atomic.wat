(module
  (memory 1 1 shared)

  (func
    (i32.atomic.rmw.cmpxchg
      (i32.const 0)
      (i32.const 1)
      (i32.const 2))

    (i32.atomic.rmw.add
      (i32.const 0)
      (i32.const 1))

    (atomic.notify
      (i32.const 0)
      (i32.const 1))

    (i32.atomic.wait
      (i32.const 0)
      (i32.const 1)
      (i64.const 2)
      )

    (i64.atomic.wait
      (i32.const 0)
      (i64.const 1)
      (i64.const 2)
      )

    i32.add
    i32.add
    i32.add
    i32.add
    drop
  )
)

;; CHECK: (func
;; NEXT:    (block
;; NEXT:      (drop
;; NEXT:        (I32Add
;; NEXT:          (cmpxchg 0
;; NEXT:            (const 0)
;; NEXT:            (const 1)
;; NEXT:            (const 2)
;; NEXT:          )
;; NEXT:          (I32Add
;; NEXT:            (atomic.rmw 0
;; NEXT:              (const 0)
;; NEXT:              (const 1)
;; NEXT:            )
;; NEXT:            (I32Add
;; NEXT:              (atomic.notify 0
;; NEXT:                (const 0)
;; NEXT:                (const 1)
;; NEXT:              )
;; NEXT:              (I32Add
;; NEXT:                (atomic.wait 0
;; NEXT:                  (const 0)
;; NEXT:                  (const 1)
;; NEXT:                  (const 2)
;; NEXT:                )
;; NEXT:                (atomic.wait 0
;; NEXT:                  (const 0)
;; NEXT:                  (const 1)
;; NEXT:                  (const 2)
;; NEXT:                )
;; NEXT:              )
;; NEXT:            )
;; NEXT:          )
;; NEXT:        )
;; NEXT:      )
;; NEXT:    )
;; NEXT:  )
