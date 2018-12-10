(module
  (type (;0;) (func (result i32)))
  (func $f (type 0) (local i32)
    (set_local 0 (i32.const 9))
    loop
      (br_if 0 (i32.eqz (get_local 0)))
      (set_local 0 (i32.add (get_local 0) (i32.const 1)))
    end
    i32.const 10)
  (export "count_to_ten" (func $f)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (result i32)))
;; NEXT:    (func (;0;) (type 0) (result i32)
;; NEXT:      (local i32)
;; NEXT:      i32.const 9
;; NEXT:      set_local 0
;; NEXT:      loop  ;; label = @1
;; NEXT:        get_local 0
;; NEXT:        i32.eqz
;; NEXT:        br_if 0 (;@1;)
;; NEXT:        get_local 0
;; NEXT:        i32.const 1
;; NEXT:        i32.add
;; NEXT:        set_local 0
;; NEXT:      end
;; NEXT:      i32.const 10)
;; NEXT:    (export "count_to_ten" (func 0)))
