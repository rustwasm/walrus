(module
  (table 1 anyref)
  (table 1 anyref)
  (func (export "a") (param anyref) (result anyref)

    i32.const 0
    local.get 0
    table.set 0

    i32.const 0
    local.get 0
    table.set 1

    i32.const 0
    i32.const 0
    table.get 0
    table.set 1

    i32.const 0
    i32.const 0
    table.get 1
    table.set 0

    i32.const 0
    ref.null
    table.grow 0
    drop

    i32.const 0
    ref.null
    table.grow 1
    drop

    ref.null
    ref.is_null
    i32.const 0
    i32.add
    drop

    table.size 0
    drop

    table.size 1
    drop

    i32.const 0
    table.get 0))

(; CHECK-ALL:
  (module
    (type (;0;) (func (param anyref) (result anyref)))
    (func (;0;) (type 0) (param anyref) (result anyref)
      i32.const 0
      local.get 0
      table.set 0
      i32.const 0
      local.get 0
      table.set 1
      i32.const 0
      i32.const 0
      table.get 0
      table.set 1
      i32.const 0
      i32.const 0
      table.get 1
      table.set 0
      i32.const 0
      ref.null
      table.grow 0
      drop
      i32.const 0
      ref.null
      table.grow 1
      drop
      ref.null
      ref.is_null
      i32.const 0
      i32.add
      drop
      table.size 0
      drop
      table.size 1
      drop
      i32.const 0
      table.get 0)
    (table (;0;) 1 anyref)
    (table (;1;) 1 anyref)
    (export "a" (func 0)))
;)
