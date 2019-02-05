(module
  (func $wat)
  (export "another" (func $wat)))

;; CHECK: (func $wat
