(module
  (table 1 anyref)
  (func (export "a") (param i32) (result anyref)
    local.get 0
    table.get 0))

(; CHECK-ALL:
  (module
    (type (;0;) (func (param i32) (result anyref)))
    (func (;0;) (type 0) (param i32) (result anyref)
      local.get 0
      table.get 0)
    (table (;0;) 1 anyref)
    (export "a" (func 0)))
;)
