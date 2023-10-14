;; Taken from `spec-tests/proposals/multi-value/fac.wast`.

(module
  ;; Iterative factorial without locals.
  (func $pick0 (param i64) (result i64 i64)
    (local.get 0) (local.get 0)
  )
  (func $pick1 (param i64 i64) (result i64 i64 i64)
    (local.get 0) (local.get 1) (local.get 0)
  )
  (func (export "fac-ssa") (param i64) (result i64)
    (i64.const 1) (local.get 0)
    (loop $l (param i64 i64) (result i64)
      (call $pick1) (call $pick1) (i64.mul)
      (call $pick1) (i64.const 1) (i64.sub)
      (call $pick0) (i64.const 0) (i64.gt_u)
      (br_if $l)
      (drop) (return)
    )
  )
)
