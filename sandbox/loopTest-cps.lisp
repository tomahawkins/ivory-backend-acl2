( set-irrelevant-formals-ok t)

( set-ignore-ok t)

( defun call-cont ( stack heap retval)
  ( if ( consp stack)
    ( if ( equal ( car stack) "_cont_0")
      ( let*
        ( ( stack ( cdr stack))
          ( ref1 ( car stack))
          ( stack ( cdr stack))
          ( deref4 ( nth ref1 heap))
          ( _cps0 1)
        )
        ( call-cont stack heap deref4)
      )
      retval
    )
    retval
  )
)

( defun _cps10 ( stack heap ix2 _cps7 ref1)
  ( declare ( xargs :measure ( nfix ( + 1 ( - ix2 _cps7)))))
  ( if ( and ( and ( and t ( integerp ix2)) ( integerp _cps7)) ( integerp ref1))
    ( let*
      ( ( _cps11 ( if ( >= ix2 _cps7) 1 0)))
      ( if ( zip _cps11)
        ( let*
          ( ( deref4 ( nth ref1 heap))
            ( _cps0 1)
          )
          ( call-cont stack heap deref4)
        )
        ( let*
          ( ( deref3 ( nth ref1 heap))
            ( _cps8 1)
            ( _cps9 ( + deref3 _cps8))
            ( heap ( append ( take ref1 heap) ( cons _cps9 ( nthcdr ( + ref1 1) heap))))
            ( _cps12 1)
            ( ix2 ( - ix2 _cps12))
          )
          ( _cps10 stack heap ix2 _cps7 ref1)
        )
      )
    )
    nil
  )
)

( defun loopTest ( stack heap var0)
  ( if ( and t ( integerp var0))
    ( let*
      ( ( ref1 ( len heap))
        ( heap ( append heap ( cons 0 nil)))
        ( stack ( cons ref1 stack))
      )
      ( _cps10 ( cons "_cont_0" stack) heap (mod (- var0 1) 10) (mod 0 10) ref1)
    )
    nil
  )
)

(loopTest nil nil 3)
