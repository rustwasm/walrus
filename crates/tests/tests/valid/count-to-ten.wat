(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (local i32)
    (local.set 0 (i32.const 9))
    loop
      (br_if 0 (i32.eqz (local.get 0)))

      (local.set 0 (i32.add (local.get 0) (i32.const 1)))
    end
    i32.const 10)
  (export "count_to_ten" (func 0)))
