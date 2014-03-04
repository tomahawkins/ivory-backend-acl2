(mutual-recursion

  ; Pop the continuation name off the stack and call it with the return value, else return the return value.
  (defun call-cont (stack retval)
    ;(declare (xargs :measure (len stack)))
    (if (equal (car stack) "factorial-cont")
      (factorial-cont (cdr stack) retval)
      retval
    )
  )

  ; The continuation of the factorial function.  Restores the input argument 'n' and multiplies it with the result of the recursive call.
  (defun factorial-cont (stack retval)
    ;(declare (xargs :measure (len stack)))
    (let*
      ( (n     (car stack))  ; Restore the input argument.
        (stack (cdr stack))  ; Update the stack.
      )
      (call-cont stack (* n retval))
    )
  )

  ; The factorial function takes the stack and the number to compute the factorial.
  (defun factorial (stack n)
    ;(declare (xargs :measure n))
    (if (> n 1)
      (factorial (cons "factorial-cont" (cons n stack)) (- n 1))  ; On the recursive call, push 'n' and continuation name onto the stack.
      (call-cont stack n)
    )
  )
)

(defun main (n) (factorial nil n))

