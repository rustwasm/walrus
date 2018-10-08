(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0)
    get_local 0
    if (result i32)
      i32.const 1
    else
      i32.const 2
    end)
  (export "if_else" (func 0)))
