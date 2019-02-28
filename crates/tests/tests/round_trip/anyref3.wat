(module
  (type (func))
  (table 1 anyfunc)
  (table 1 anyref)
  (func (export "a") (param i32) (result anyref)
    local.get 0
    call_indirect (type 0)
    local.get 0
    table.get 1))

(; CHECK-ALL:
  (module
    (type (;0;) (func))
    (type (;1;) (func (param i32) (result anyref)))
    (func (;0;) (type 1) (param i32) (result anyref)
      local.get 0
      call_indirect (type 0)
      local.get 0
      table.get 1)
    (table (;0;) 1 funcref)
    (table (;1;) 1 anyref)
    (export "a" (func 0)))
;)
