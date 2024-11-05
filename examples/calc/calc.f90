program calc ! Command line complex calculator
    use evaluate_kinds
    use evaluate_strings
    use evaluate

    character(len=132) :: str, line, ustr
    character(len=24) :: varname
    complex(c8) :: valcd
    integer :: lstr, ipos

    evalerr = 0
    write (*, *)
    write (*, *) '   Welcome to the command line complex calculator. '
    write (*, *) '   The program can be exited at any time by hitting'
    write (*, *) "   'Enter' at the prompt without entering any data."
    write (*, *) "   Help information can be obtained by typing 'help' at"
    write (*, *) "   the prompt or by reading the file 'CalcReadMe.txt'."

    do
        write (*, '(/a)', advance='NO') ' >> ' ! Command line prompt
        read (*, '(a)') str
        ustr = uppercase(str)
        lstr = len_trim(str)
        if (lstr == 0) exit
        if (trim(adjustl(ustr)) == 'LIST') then
            call listvar ! List all variables and their values
            cycle
        end if
        if (trim(adjustl(ustr)) == 'HELP') then
            write (*, "(/'*********************'//   &
        &    '    Entering an equation such as'//  &
        &    '    >> var=1.57e-6'//  &
        &    '    assigns the value 1.57e-6 to the variable var. Entering'/  &
        &    '    an expression such as'//  &
        &    '    >> sqrt(cos(x))'//  &
        &    '    gives the value of the expression in the form'//  &
        &    '    result=(<real>,<imag>).'//  &
        &    '    The last calculated result is stored in the variable result.'/  &
        &    '    An expression can contain the operators +, -, *, /, ^,'/  &
        &    '    as well as nested parentheses for grouping. An expression'/  &
        &    '    can also contain previously defined variables as well as'/   &
        &    '    the functions sin, cos, tan, log, ln, exp, sqrt, abs, ang,'/  &
        &    '    real, imag, and conjg. The variables pi and i (imaginary unit)'/  &
        &    '    are predefined. Complex numbers can be entered as a+i*b or'/   &
        &    '    complex(a,b). To list all variables and their values type in'//  &
        &    '    >> list'//&
        &    '*********************'/)")
            cycle
        end if
        ipos = index(str, '=')
        if (ipos == 0) then
            call evalexpr(str, valcd) ! Evaluate expression
            if (evalerr == 0 .or. evalerr == 9) then
                call defparam('result', valcd) ! Make result a stored variable
                if (aimag(valcd) == 0.) then
                    write (*, *)
                    write (*, *) 'Result = ', real(valcd)
                else
                    write (*, *)
                    write (*, *) 'Result = ', valcd ! Output result to screen
                end if
            end if
        else
            call evaleqn(str) ! Evaluate equality
        end if
    end do

contains

    subroutine evaleqn(eqn) ! Evaluate an equation
        integer :: nargs
        character(*) :: eqn
        character(len(eqn)) :: args(2)
        complex(c8) :: val

        call parse(eqn, '=', args, nargs) ! Seperate right- and left-hand-sides
        call defparam(adjustl(args(1)), args(2)) ! Evaluate right-hand-side and
        ! assign to symbol on the
        ! left-hand-side.
    end subroutine evaleqn

end program calc

