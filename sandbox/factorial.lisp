(mutual-recursion
  ; Pop the continuation name off the stack and call it with the return value, else return the return value.
  (defun call-cont (stack retval)
    (if (and (consp stack) (equal (car stack) "factorial-cont"))
      (factorial-cont (cdr stack) retval)
      retval
    )
  )

  ; The continuation of the factorial function.  Restores the input argument 'n' from the stack
  ; and multiplies it with the result of the recursive call.
  (defun factorial-cont (stack retval)
    (if (consp stack)
      (call-cont (cdr stack) (* (car stack) retval))
      retval
    )
  )
)

; The factorial function takes the stack and the number to compute the factorial.
(defun factorial (stack n)
  (if (and (integerp n) (> n 1))
    ; On the recursive call, push 'n' and the continuation name onto the stack.
    (factorial (cons "factorial-cont" (cons n stack)) (- n 1))
    (call-cont stack n)
  )
)

(defun main (n) (factorial nil n))

