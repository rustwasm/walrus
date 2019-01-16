(module
  (type (;0;) (func (param i32) (result i32)))
  (func $fac (type 0) (local i32)
    block
      local.get 0
      local.set 1
      loop
        ;; if local 0 == 0, break
        local.get 0
        i32.eqz
        br_if 1

        ;; local 1 = local 0 * local 1
        local.get 1
        local.get 0
        i32.mul
        local.set 1

        ;; local 0 = local 0 - 1
        local.get 0
        i32.const 1
        i32.sub
        local.set 0
      end
    end

    ;; return the accumulated value
    local.get 1)
  (export "fac" (func $fac)))

;; CHECK: (module
;; NEXT:    (type (;0;) (func (param i32) (result i32)))
;; NEXT:    (func (;0;) (type 0) (param i32) (result i32)
;; NEXT:      (local i32 i32)
;; NEXT:      block  ;; label = @1
;; NEXT:        local.get 1
;; NEXT:        local.set 0
;; NEXT:        loop  ;; label = @2
;; NEXT:          local.get 1
;; NEXT:          i32.eqz
;; NEXT:          br_if 1 (;@1;)
;; NEXT:          local.get 0
;; NEXT:          local.get 1
;; NEXT:          i32.mul
;; NEXT:          local.set 0
;; NEXT:          local.get 1
;; NEXT:          i32.const 1
;; NEXT:          i32.sub
;; NEXT:          local.set 1
;; NEXT:        end
;; NEXT:      end
;; NEXT:      local.get 0)
;; NEXT:    (export "fac" (func 0)))
