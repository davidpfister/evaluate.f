!> @defgroup group_stacks evaluate_stacks
!> @brief Stacks module
!! It contains the @link evaluate_stacks::valstack valstack @endlink and 
!! @link evaluate_stacks::opstack opstack @endlink stacks to store the 
!! operand values and the operator values, respectively.
!! @cond
#ifndef EVALUTE_STACKSIZE
#define EVALUTE_STACKSIZE 100
#endif
!! @endcond
module evaluate_stacks
    use evaluate_kinds
    
    implicit none; private
    
    !> @class item
    !! @ingroup group_stacks
    !! @brief   Provides a simple class for describing operation items.
    !! @verbatim type, private :: item @endverbatim
    !! <h2>Examples</h2>
    !! The following example demonstrates some of the main members of the 
    !! @link evaluate_stacks::item item @endlink class.
    !! @n
    !! @code
    !! type(item) :: token
    !! token%char = 'COS'
    !! token%type = 'F' !Function tocken
    !! @endcode
    !! @par
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @link evaluate_stacks::item item @endlink class
    !! <h3>item(character(24) char, character(1) type)</h3>
    !! 
    !! @param[in] char the string representation of the token
    !! @param[in] type the type of the token
    !! 
    !! @b Examples
    !! @code{.f90}
    !!  type(item) :: token
    !!  token = item('*', 'B')
    !! @endcode
    !! @note The types accepted when tokenizing the expression are: `E`, `S`, `B`, `U` and `F`
    !! - E: end token
    !! - S: operand token
    !! - B: binary operation (`^`, `*`, `/`, `+`, `-`)
    !! - U: unary operator (`+`, `-`)
    !! - F: function token (`sin`, `cos`, `tan`, `log10`, `log`, `abs`, `exp`, `sqrt`, `real`, `imag`, `conjg`, and `ang`)
    !! @b Remarks
    type, public :: item
    !! @cond
        character(24) :: char
        character(1) :: type
    !! @endcond
    end type
    
    !! @private
    integer, public             :: itop, ibin !< indexes in the stack
    !! @private
    integer, parameter, public  :: numtok = EVALUTE_STACKSIZE !< Maximum number of tokens to size the stacks

    !> @name Variables
    !! @{

    !> @brief Represent a fixed length last-in-first-out (LIFO) collection of complex (c8) values. 
    !! It contains all the the values used for the estimation of the expression. Its capacity is
    !! set equal to the preprocessor macro `EVALUTE_STACKSIZE`. The default value is 100. 
    !> @ingroup group_stacks
    complex(c8), public         :: valstack(numtok)

    !> @brief Represent a fixed length last-in-first-out (LIFO) collection of 
    !! @link evaluate_stacks::item item @endlink class.
    !! It contains all the the operations encountered during the parsing of the expression
    !! and used for the estimation of the expression. Its capacity is
    !! set equal to the preprocessor macro `EVALUTE_STACKSIZE`. The default value is 100.
    !! @note To modify the stack size, one should change the value of `EVALUTE_STACKSIZE`
    !! at compile-time `fpm build '-D_EVALUTE_STACKSIZE = 200'`.
    type(item), public          :: opstack(numtok) ! Operator stack used in conversion to postfix
    !> @}

    public :: pushop, &
              popop, & 
              pushval, &
              popval
    
    contains
    
    !> @brief   Push the operator to the stack
    !! @ingroup group_stacks
    !! @param[in] op operator to be added to the @link evaluate_stacks::opstack opstack @endlink 
    !!
    !! @b Remarks
    subroutine pushop(op)
        type(item), intent(in) :: op

        itop = itop + 1
        if (itop > numtok) then
            write (*, *) 'Error: operator stack overflow in evaluation of expression'
            write (*, *)
            return
        end if
        opstack(itop) = op
    end subroutine

    !> @brief   Takes top operator of operator stack and assigns it to op
    !! @ingroup group_stacks
    !! @param[in] op first operator from the @link evaluate_stacks::opstack opstack @endlink 
    !!
    !! @b Remarks
    subroutine popop(op)
        type(item), intent(inout):: op

        op = opstack(itop)
        itop = itop - 1
    end subroutine

    !> @brief   Puts value on value stack
    !! @ingroup group_stacks
    !! @param[in] val value to be added to the @link evaluate_stacks::valstack valstack @endlink 
    !!
    !! @b Remarks
    subroutine pushval(val)
        complex(c8) :: val

        itop = itop + 1
        if (itop > numtok) then
            write (*, *) 'Error: value stack overflow in evaluation of expression'
            write (*, *)
            return
        end if
        valstack(itop) = val
    end subroutine

    !> @brief   Takes top value off value stack and assigns it to val
    !! @ingroup group_stacks
    !! @param[in] val first value from the @link evaluate_stacks::valstack valstack @endlink 
    !!
    !! @b Remarks
    subroutine popval(val)

        complex(c8) :: val

        val = valstack(itop)
        itop = itop - 1

    end subroutine
    
end module