(module
  (type (func (param i32 f32)))
  (import "spectest" "print_i32_f32" (func $print_i32_f32 (param i32 f32)))
  (table 1 anyfunc)

  (func (export "print32")
    (call $print_i32_f32
      (i32.const 1)
      (f32.const 42)
    )

    i32.const 2
    f32.const 43
    i32.const 0
    call_indirect (type 0)
  )

)
;; CHECK: (call 0
;; NEXT:    (const 1)
;; NEXT:    (const 42)

;; CHECK: (call.indirect 0 0
;; NEXT:    (const 0)
;; NEXT:    (const 2)
;; NEXT:    (const 43)

