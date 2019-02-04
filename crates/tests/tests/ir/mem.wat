(module
  (memory 0)
  (func
    i32.const 1
    i32.load
    drop

    i32.const 2
    f32.const 3
    f32.store))

;; CHECK: (load 0
;; NEXT:    (const 1)

;; CHECK: (store 0
;; NEXT     (const 2)
;; NEXT     (const 3)
