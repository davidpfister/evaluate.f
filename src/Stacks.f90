module evaluate_stacks
    use evaluate_kinds
    
    implicit none; private
    
    type, public :: item
        character(24) :: char
        character :: type
    end type
    
    integer, public             :: itop, ibin
    integer, parameter, public  :: numtok = 100 ! Maximum number of tokens
    complex(c8), public         :: valstack(numtok) ! Stack used in evaluation of expression
    type(item), public          :: opstack(numtok) ! Operator stack used in conversion to postfix
    
    public :: pushop, &
              popop, & 
              pushval, &
              popval
    
    contains
    
    subroutine pushop(op) ! Puts an operator on operator stack
        type(item):: op

        itop = itop + 1
        if (itop > numtok) then
            write (*, *) 'Error: operator stack overflow in evaluation of expression'
            write (*, *)
            return
        end if
        opstack(itop) = op
    end subroutine

    subroutine popop(op) ! Takes top operator of operator stack and assigns it to op

        type(item):: op

        op = opstack(itop)
        itop = itop - 1
    end subroutine

    subroutine pushval(val) ! Puts value on value stack
        complex(c8) :: val

        itop = itop + 1
        if (itop > numtok) then
            write (*, *) 'Error: value stack overflow in evaluation of expression'
            write (*, *)
            return
        end if
        valstack(itop) = val
    end subroutine

    subroutine popval(val) ! Takes top value off value stack and assigns it to val

        complex(c8) :: val

        val = valstack(itop)
        itop = itop - 1

    end subroutine
    
end module