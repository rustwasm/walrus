(module
  (func (export "multiLoop") (param i32 i64 i64) (result i64 i64)
    (local.get 2)
    (local.get 1)
    (local.get 0)
    (if (param i64 i64) (result i64 i64)
      (then return)
      (else
        (drop)
        (drop)
        (i64.const 0)
        (i64.const 0)))))

;; CHECK: todo
