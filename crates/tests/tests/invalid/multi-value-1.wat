(module
  (func (export "i64.dup") (param i64) (result i64 i64)
    ;; Too many i64s on the stack.
    get_local 0
    get_local 0
    get_local 0))
