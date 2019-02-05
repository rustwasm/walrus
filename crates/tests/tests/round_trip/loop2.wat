(module
  (func $dummy)
  (func (export "as-loop-last") (param i32)
    (loop (call $dummy) (br_if 1 (local.get 0)))
  ))

;; CHECK:    (module
;; NEXT:       (type (;0;) (func))
;; NEXT:       (type (;1;) (func (param i32)))
;; NEXT:       (func (;0;) (type 1) (param i32)
;; NEXT:         loop  ;; label = @1
;; NEXT:           call $dummy
;; NEXT:           local.get 0
;; NEXT:           br_if 1 (;@0;)
;; NEXT:         end)
;; NEXT:       (func $dummy (type 0))
;; NEXT:       (export "as-loop-last" (func 0)))
