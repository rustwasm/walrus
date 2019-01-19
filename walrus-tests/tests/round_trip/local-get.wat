
(module
  (func (export "as-br-value") (param i32) (result i32)
    (block (result i32) (br 0 (local.get 0)))
  )
)

;; CHECK wut
