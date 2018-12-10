(module
  (type (;0;) (func (param i32) (result i32)))
  (func $fac (type 0) (local i32)
    block
      get_local 0
      set_local 1
      loop
        ;; if local 0 == 0, break
        get_local 0
        i32.eqz
        br_if 1

        ;; local 1 = local 0 * local 1
        get_local 1
        get_local 0
        i32.mul
        set_local 1

        ;; local 0 = local 0 - 1
        get_local 0
        i32.const 1
        i32.sub
        set_local 0
      end
    end

    ;; return the accumulated value
    get_local 1)
  (export "fac" (func $fac)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (param i32) (result i32)))
;; NEXT:    (func (;0;) (type 0) (param i32) (result i32)
;; NEXT:      (local i32 i32)
;; NEXT:      block  ;; label = @1
;; NEXT:        get_local 0
;; NEXT:        set_local 1
;; NEXT:        loop  ;; label = @2
;; NEXT:          get_local 0
;; NEXT:          i32.eqz
;; NEXT:          br_if 1 (;@1;)
;; NEXT:          get_local 1
;; NEXT:          get_local 0
;; NEXT:          i32.mul
;; NEXT:          set_local 1
;; NEXT:          get_local 0
;; NEXT:          i32.const 1
;; NEXT:          i32.sub
;; NEXT:          set_local 0
;; NEXT:        end
;; NEXT:      end
;; NEXT:      get_local 1)
;; NEXT:    (export "fac" (func 0)))
