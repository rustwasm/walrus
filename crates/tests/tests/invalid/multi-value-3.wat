(module
  (func (export "multiBlock") (param i64 i64) (result i64 i64 i64)
    ;; Too many i64s on the stack for the block.
    (local.get 0)
    (local.get 1)
    (local.get 0)
    (block (param i64 i64) (result i64 i64 i64)
      (i64.const 1234))))
