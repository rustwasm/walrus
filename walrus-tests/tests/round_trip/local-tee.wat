(module
  (func (export "x")
    (local i32)
    i32.const 0
    local.tee 0
    local.set 0))

;; CHECK: local.tee 0
