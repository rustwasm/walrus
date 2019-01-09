(module
  (type (;0;) (func (result i32)))
  (import "env" "f" (func $f (type 0)))
  (func $g (type 0) (result i32)
    (call $f))
  (export "g" (func $g)))
