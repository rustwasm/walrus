(module
  (func (export "f") (param i32) (result i32)
    (local.get 0)
    (if (result i32)
      (then (i32.const 1))
      ;; Note: since the `if` block is supposed to produce a result, we need an
      ;; `else` here to produce that result if the condition is false.
      ;;
      ;; (else (i32.const 2))
    )
  )
)
