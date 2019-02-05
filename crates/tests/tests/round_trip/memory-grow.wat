(module
  (memory 0)
  (func (export "a")
    i32.const 0
    memory.grow
    drop))

;; CHECK: memory.grow
