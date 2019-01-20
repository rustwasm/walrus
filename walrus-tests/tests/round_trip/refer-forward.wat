(module
  (func (export "b") call $a)
  (func $a)
  )

;; CHECK: call $a
