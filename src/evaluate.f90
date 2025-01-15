!> @defgroup group_evaluate evaluate
!> @brief Evaluate module
!! @{
module evaluate
    use evaluate_value
    use evaluate_kinds
    use evaluate_parameters
    use evaluate_string
    use evaluate_stacks

    private

    public :: eval, defparam, listvar, uppercase
    public :: i1, i2, i4, i8, r4, r8, r16, c4, c8, c16

    !> @interface eval
    !> @brief Evaluate expression expr for
    !! val numerics
    interface eval
    !! @cond
        module procedure eval_c8 ! Double precision complex result
        module procedure eval_c4 ! Single precision complex result
        module procedure eval_r8 ! Double precision real result
        module procedure eval_r4 ! Single precision real result
        module procedure eval_i8 ! Double precision integer result
        module procedure eval_i4 ! Single precision integer result
    !! @endcond
    end interface
    
    !> @interface defparam
    !> @brief Associates sym with val in symbol table,
    !!        val double precision complex
    !! @verbatim defparam(sym, val, ierr) @endverbatim
    !! @param[in] sym parameter symbol
    !! @param[in] val parameter value
    !! @param[out] ierr error code
    interface defparam
    !! @cond
        module procedure defparam_char ! value given by expression
        module procedure defparam_c8 ! Double precision complex value
        module procedure defparam_c4 ! Single precision complex value
        module procedure defparam_r8 ! Double precision real value
        module procedure defparam_r4 ! Single precision real value
        module procedure defparam_i8 ! Double precision integer value
        module procedure defparam_i4 ! Single precision integer value
    !! @endcond
    end interface

    contains
    
    !> @brief Evaluate expression expr for
    !! val double precision complex
    subroutine eval_c8(expr, val, ierr) 
        character(*), intent(in)        :: expr
        complex(c8), intent(out)        :: val
        integer, intent(out), optional  :: ierr
        !private
        character(len(expr) + 1) :: tempstr
        character :: cop
        integer :: isp(numtok) ! On stack priority of operators in opstack
        integer :: lstr
        complex(c8) :: cval, oper1, oper2
        real(r8) :: valr, vali
        type(item):: token(numtok) ! List of tokens ( a token is an operator or
                                   ! operand) in postfix order
        type(item) :: x, junk, tok
        integer :: i, icp, insp, isum, ntok, ios
        
        if (present(ierr)) ierr = 0
        token(1:)%char = ' '

        if (len_trim(expr) == 0) then ! Expression empty
            if (present(ierr)) ierr = 1
            write (*, *) 'Error: expression being evaluated is empty'
            return
        end if

        tempstr = adjustl(expr)
        call removesp(tempstr) ! Removes spaces, tabs, and control characters

        ! STEP 1:  Convert string to token array. Each token is either an operator or
        !          an operand. Token array will be in postfix (reverse Polish) order.

        ntok = 0
        ibin = 0
        itop = 0
        do
            lstr = len_trim(tempstr)
            call get_next_token(tempstr(1:lstr), tok, icp, insp)
            select case (tok%type)
            case ('S')
                ntok = ntok + 1
                token(ntok) = tok
            case ('E')
                do
                    if (itop < 1) exit
                    call popop(x) ! Output remaining operators on stack
                    ntok = ntok + 1
                    token(ntok) = x
                end do
                ntok = ntok + 1
                token(ntok) = tok
                exit
            case ('R') ! Token is right parenenthesis
                do
                    if (itop <= 0) exit
                    if (opstack(itop)%type == 'L') exit ! Output operators on stack down
                    call popop(x) ! to left parenthesis
                    ntok = ntok + 1
                    token(ntok) = x
                end do
                call popop(junk) ! Remove left parenthesis from stack
                if (itop > 0) then
                    if (opstack(itop)%type == 'F') then ! Output function name if present
                        call popop(x)
                        ntok = ntok + 1
                        token(ntok) = x
                    end if
                end if
            case ('D') ! Token is comma
                do
                    if (itop <= 0) exit
                    if (opstack(itop)%type == 'L') exit ! Output operators on stack down
                    call popop(x) ! to left parenthesis
                    ntok = ntok + 1
                    token(ntok) = x
                end do
            case ('U', 'B', 'L', 'F') ! Token is operator, left parenthesis or function name
                do
                    if (itop <= 0) exit ! Output operators on stack having
                    if (isp(itop) < icp) exit
                    call popop(x) ! an instack priority that is
                    ntok = ntok + 1 ! greater than or equal to the
                    token(ntok) = x ! priority of the incoming operator
                end do
                call pushop(tok) ! Put incoming operator on stack
                isp(itop) = insp
            end select
        end do

        isum = 0 ! Error check for matching parentheses
        do i = 1, ntok
            if (token(i)%type == 'L') isum = isum + 1
            if (token(i)%type == 'R') isum = isum - 1
        end do
        if (isum /= 0) then
            if (present(ierr)) ierr = 2
            write (*, *) 'Error in the evaluation of the expression ', trim(expr)
            write (*, *) "Parentheses don't match"
            write (*, *)
            return
        end if

        ! STEP 2: Evaluate token string in postfix order

        itop = 0
        i = 0
        do while (i < ntok)
            i = i + 1
            x = token(i)
            select case (x%type)
            case ('E') ! Token is end token
                if (itop > 1) then
                    if (present(ierr)) ierr = 12
                    write (*, *) 'Error: missing operator in expression ', trim(expr)
                    write (*, *)
                    return
                end if
                call popval(val) ! Final result left on stack of values
                exit
            case ('S') ! Token is operand
                call getvalue(x%char, cval, ios) ! Evaluate operand
                if (ios /= 0) then
                    if (present(ierr)) ierr = ios
                    return
                end if
                call pushval(cval) ! Put value of operand on stack
            case ('B') ! Token is a binary operator
                if (itop < 2) then
                    if (present(ierr)) ierr = 5
                    write (*, *) 'Error in evaluation of expression ', trim(expr)
                    write (*, *) 'Less than two operands for binary operator  ' &
                        , trim(x%char)
                    write (*, *)
                    return
                end if
                call popval(oper1) ! Pull off top two values from stack
                call popval(oper2)
                select case (trim(x%char)) ! Perform operation on values
                case ('^')
                    cval = oper2**oper1
                case ('*')
                    cval = oper2*oper1
                case ('/')
                    if (oper1 == (0._r8, 0._r8)) then
                        if (present(ierr)) ierr = 10
                        write (*, *) 'Error in expression ', trim(expr)
                        write (*, *) 'Division by zero'
                        write (*, *)
                        return
                    end if
                    cval = oper2/oper1
                case ('+')
                    cval = oper2 + oper1
                case ('-')
                    cval = oper2 - oper1
                end select
                call pushval(cval) ! Put result back on stack
            case ('U') ! Token is unary operator
                if (itop == 0) then
                    if (present(ierr)) ierr = 6
                    write (*, *) 'Error in expression ', trim(expr)
                    write (*, *) 'No operand for unary operator ', trim(x%char)
                    write (*, *)
                    return
                else
                    call popval(oper1) ! Pull top value off stack
                end if
                select case (trim(x%char)) ! Operate on value
                case ('+')
                    cval = oper1
                case ('-')
                    cval = -oper1
                end select
                call pushval(cval) ! Put result back on stack
            case ('F') ! Token is a function name
                if (itop == 0) then
                    if (present(ierr)) ierr = 7
                    write (*, *) 'Error in expression ', trim(expr)
                    write (*, *) 'Missing argument(s) for function ', trim(x%char)
                    write (*, *)
                    return
                else
                    call popval(oper1) ! Pull top value off stack
                end if
                tempstr = uppercase(x%char)
                select case (trim(tempstr)) ! Evaluate function
                case ('SIN')
                    cval = sin(oper1)
                case ('COS')
                    cval = cos(oper1)
                case ('TAN')
                    oper2 = cos(oper1)
                    if (abs(oper2) == 0.0_r8) then
                        if (present(ierr)) ierr = 14
                        write (*, *) 'Error: argument of tan function a multiple', &
                            ' of pi/2 in expression ', trim(expr)
                        write (*, *)
                        return
                    else
                        cval = sin(oper1)/oper2
                    end if
                case ('SINH')
                    cval = sinh(oper1)
                case ('COSH')
                    cval = cosh(oper1)
                case ('TANH')
                    cval = tanh(oper1)
                case ('ACOS')
                    cval = acos(oper1)
                case ('ASIN')
                    cval = asin(oper1)
                case ('ATAN')
                    cval = atan(oper1)
                case ('SQRT')
                    if (real(oper1, r8) < 0. .and. aimag(oper1) == 0.) then
                        if (present(ierr)) ierr = 9
                        write (*, *) 'Warning: square root of negative real number', &
                            ' in expression ', trim(expr)
                        write (*, *)
                    end if
                    cval = sqrt(oper1)
                case ('ABS')
                    cval = abs(oper1)
                case ('LOG')
                    if (real(oper1, r8) <= 0. .and. aimag(oper1) == 0.) then
                        if (present(ierr)) ierr = 8
                        write (*, *) 'Error: negative real or zero argument for', &
                            ' natural logarithm in expression ', trim(expr)
                        write (*, *)
                        return
                    end if
                    cval = log(oper1)
                case ('LOG10')
                    if (real(oper1, r8) <= 0. .and. aimag(oper1) == 0.) then
                        if (present(ierr)) ierr = 8
                        write (*, *) 'Error: negative real or zero argument for base', &
                            '10 logarithm in expression ', trim(expr)
                        write (*, *)
                        return
                    end if
                    cval = log(oper1)/2.30258509299405_r8
                case ('EXP')
                    cval = exp(oper1)
                case ('COMPLEX')
                    if (itop == 0) then
                        if (present(ierr)) ierr = 7
                        write (*, *) 'Error in expression ', trim(expr)
                        write (*, *) 'Missing argument(s) for function ', trim(x%char)
                        write (*, *)
                        return
                    else
                        call popval(oper2) ! Pull second argument off stack
                    end if
                    valr = real(oper2, r8)
                    vali = real(oper1, r8)
                    cval = cmplx(valr, vali, c8)
                case ('CONJG')
                    cval = conjg(oper1)
                case ('ANG')
                    cval = atan2(aimag(oper1), real(oper1, r8))
                case ('REAL')
                    cval = real(oper1, r8)
                case ('IMAG')
                    cval = aimag(oper1)
                case default ! Undefined function
                    if (present(ierr)) ierr = 13
                    write (*, *) 'Error: the function ', trim(x%char), ' is undefined', &
                        ' in the expression ', trim(expr)
                    write (*, *)
                    return
                end select
                call pushval(cval) ! Put result back on stack
            end select
        end do

    end subroutine

    !> @brief Evaluate expression expr for
    !! val single precision complex
    subroutine eval_c4(expr, val, ierr) 
        character(*), intent(in)        :: expr
        complex(c4), intent(out)        :: val
        integer, intent(out), optional  :: ierr
        !private
        complex(c8) :: vald

        call eval_c8(expr, vald, ierr)
        val = vald
    end subroutine

    !> @brief Evaluate expression expr for
    !! val single precision real
    subroutine eval_r4(expr, val, ierr) 
        character(*), intent(in)        :: expr
        real(r4), intent(out)           :: val
        integer, intent(out), optional  :: ierr
        !private
        complex(c8) :: vald

        call eval_c8(expr, vald, ierr)
        val = real(vald)
    end subroutine

    !> @brief Evaluate expression expr for
    !! val double precision real
    subroutine eval_r8(expr, val, ierr) 
        character(*), intent(in)        :: expr
        real(r8), intent(out)           :: val
        integer, intent(out), optional  :: ierr
        !private
        complex(c8) :: vald

        call eval_c8(expr, vald, ierr)
        val = real(vald, r8)
    end subroutine

    !> @brief Evaluate expression expr for
    !! ival single precision integer
    subroutine eval_i4(expr, ival, ierr) 
        character(*), intent(in)        :: expr
        integer(i4), intent(out)        :: ival
        integer, intent(out), optional  :: ierr
        !private
        complex(c8) :: vald

        call eval_c8(expr, vald, ierr)
        ival = nint(real(vald, r8), i4)
    end subroutine

    !> @brief Evaluate expression expr for
    !! ival double precision integer
    subroutine eval_i8(expr, ival, ierr) 
        character(*), intent(in)        :: expr
        integer(i8), intent(out)        :: ival
        integer, intent(out), optional  :: ierr
        !private
        complex(c8) :: vald

        call eval_c8(expr, vald, ierr)
        ival = nint(real(vald, r8), i8)
    end subroutine
    
    !> @brief Associates sym with the value of the
    !! expression expr
    subroutine defparam_char(sym, expr) 
        character(*), intent(in) :: sym, expr
        !private
        complex(c8) :: val
        integer :: ierr

        call eval_c8(expr, val, ierr) ! val is value of expression expr
        if (ierr == 0 .or. ierr == 9) then
            call defparam_c8(sym, val) ! Assign val to symbol sym
        end if
    end subroutine
        
    !! @private
    subroutine get_next_token(str, tok, icp, isp)
        character(*) :: str
        character :: cop, chtemp
        type(item) :: tok
        integer :: icp, isp, inext, ipos, lstr, ntok

        lstr = len_trim(str)
        if (lstr == 0) then
            tok%char = '#' ! Output end token
            tok%type = 'E'
            return
        end if
        ipos = scan(str, '+-*/^(),') ! Look for an arithmetic operator
        ! + - * / ^ ( ) or ,
        if (ipos > 0) cop = str(ipos:ipos)
        select case (ipos)
        case (0) ! Operators not present
            ntok = ntok + 1
            tok%char = str
            tok%type = 'S'
            str = ''
            icp = 0
            isp = 0
        case (1)
            tok%char = cop
            select case (cop)
            case ('+', '-')
                if (ibin == 0) then
                    tok%type = 'U'
                    icp = 4
                    isp = 3
                else
                    tok%type = 'B'
                    icp = 1
                    isp = 1
                end if
                ibin = 0
            case ('*', '/')
                tok%type = 'B'
                icp = 2
                isp = 2
                ibin = 0
            case ('^')
                tok%type = 'B'
                icp = 4
                isp = 3
                ibin = 0
            case ('(')
                tok%type = 'L'
                icp = 4
                isp = 0
                ibin = 0
            case (')')
                tok%type = 'R'
                icp = 0
                isp = 0
                ibin = 1
            case (',')
                tok%type = 'D'
                icp = 0
                isp = 0
                ibin = 0
            end select
            str = str(2:)
        case (2:)
            select case (cop)
            case ('(')
                tok%char = str(1:ipos - 1)
                tok%type = 'F'
                icp = 4
                isp = 0
                ibin = 0
                str = str(ipos:)
            case ('+', '-')
                chtemp = uppercase(str(ipos - 1:ipos - 1))
                if (is_letter(str(1:1)) .or. chtemp /= 'E') then
                    tok%char = str(1:ipos - 1)
                    tok%type = 'S'
                    icp = 0
                    isp = 0
                    ibin = 1
                    str = str(ipos:)
                else
                    inext = scan(str(ipos + 1:), '+-*/^(),')
                    if (inext == 0) then
                        tok%char = str
                        tok%type = 'S'
                        icp = 0
                        isp = 0
                        ibin = 0
                        str = ''
                    else
                        tok%char = str(1:ipos + inext - 1)
                        tok%type = 'S'
                        icp = 0
                        isp = 0
                        ibin = 1
                        str = str(ipos + inext:)
                    end if
                end if
            case default
                tok%char = str(1:ipos - 1)
                tok%type = 'S'
                icp = 0
                isp = 0
                ibin = 1
                str = str(ipos:)
            end select
        end select
    end subroutine
        
    !> @brief Removes spaces, tabs, control characters in
    !! string str and replace '**' by '^'
    subroutine removesp(str)
        character(*)                :: str
        !private
        character(1) :: ch
        character(len_trim(str)) :: outstr
        integer :: i, k, ich, lenstr

        str = adjustl(str)
        lenstr = len_trim(str)
        outstr = ' '
        k = 0

        do i = 1, lenstr
            ch = str(i:i)
            ich = iachar(ch)
            select case (ich)
            case (0:32) ! space, tab, or control character
                cycle
            case (33:)
                k = k + 1
                outstr(k:k) = ch
            end select
        end do
        str = replacestr(outstr, '**', '^')
    contains
        pure recursive function replacestr(string, search, substitute) result(outstr)
            character(*), intent(in)        :: string, search, substitute
            character(:), allocatable       :: outstr
            !private
            integer :: i, lenstr, lensearch

            lenstr = len(string)
            lensearch = len(search)
            if (lenstr == 0 .or. lensearch == 0) then
                outstr = ''
                return
            elseif (lenstr < lensearch) then
                outstr = string
                return
            end if
            i = 1
            do
                if (string(i:i + lensearch - 1) == search) then
                    outstr = string(1:i - 1)//substitute//&
                        replacestr(string(i + lensearch:lenstr), search, substitute)
                    exit
                end if
                if (i + lensearch > lenstr) then
                    outstr = string
                    exit
                end if
                i = i + 1
                cycle
            end do
        end function
    end subroutine

end module
!! @}