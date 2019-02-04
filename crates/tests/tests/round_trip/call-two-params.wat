(module
  (import "spectest" "print_i32_f32" (func $print_i32_f32 (param i32 f32)))

  (func (export "print32")
    (call $print_i32_f32
      (i32.const 1)
      (f32.const 42)
    )
  )
)
;; CHECK: i32.const 1
;; NEXT:  f32.const
;; NEXT:  call $print_i32_f32
