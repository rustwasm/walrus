(module
  (type (;0;) (func (param i32) (result i32)))
  (func $do_select (type 0) (param i32) (result i32)
    i32.const 2
    i32.const 1
    get_local 0
    select))

;; CHECK: (func
;; NEXT:    (block ;; e0
;; NEXT:      (select
;; NEXT:        (get_local 0)
;; NEXT:        (i32.const 1)
;; NEXT:        (i32.const 2)
;; NEXT:      )
;; NEXT:    )
;; NEXT:  )
