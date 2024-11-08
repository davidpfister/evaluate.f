module evaluate
    use evaluate_value
    use evaluate_kinds

    private

    public :: evalexpr, defparam, listvar, evalerr, uppercase
    public :: i1, i2, i4, i8, r4, r8, r16, c4, c8, c16

    type item
        character(24) :: char
        character :: type
    end type

    type param
        character(24) :: symbol
        complex(c8):: value
    end type

    interface defparam
        module procedure valdef_char ! value given by expression
        module procedure valdef_c8 ! Double precision complex value
        module procedure valdef_c4 ! Single precision complex value
        module procedure valdef_r8 ! Double precision real value
        module procedure valdef_r4 ! Single precision real value
        module procedure valdef_i8 ! Double precision integer value
        module procedure valdef_i4 ! Single precision integer value
    end interface

    interface evalexpr
        module procedure evalexpr_c8 ! Double precision complex result
        module procedure evalexpr_c4 ! Single precision complex result
        module procedure evalexpr_r8 ! Double precision real result
        module procedure evalexpr_r4 ! Single precision real result
        module procedure evalexpr_i8 ! Double precision integer result
        module procedure evalexpr_i4 ! Single precision integer result
    end interface

    interface getparam
        module procedure getparam_c8 ! Double precision complex result
        module procedure getparam_c4 ! Single precision complex result
        module procedure getparam_r8 ! Double precision real result
        module procedure getparam_r4 ! Single precision real result
        module procedure getparam_i8 ! Double precision integer result
        module procedure getparam_i4 ! Single precision integer result
    end interface

    integer, parameter  :: numtok = 100 ! Maximum number of tokens
    type(param)         :: params(100) ! Symbol table
    integer             :: nparams = 0, itop, ibin
    complex(c8)         :: valstack(numtok) ! Stack used in evaluation of expression
    type(item)          :: opstack(numtok) ! Operator stack used in conversion to postfix
    integer             :: evalerr ! Error flag

    contains
    
    !> @brief Evaluate expression expr for
    !! val double precision complex
    subroutine evalexpr_c8(expr, val) 
        character(*), intent(in) :: expr
        complex(c8) :: val
        character(len(expr) + 1) :: tempstr
        character :: cop
        integer :: isp(numtok) ! On stack priority of operators in opstack
        integer :: lstr
        complex(c8) :: cval, oper1, oper2
        real(r8) :: valr, vali
        type(item):: token(numtok) ! List of tokens ( a token is an operator or
        ! operand) in postfix order
        type(item) :: x, junk, tok
        integer :: i, icp, insp, isum, ntok
        evalerr = 0
        token(1:)%char = ' '

        if (nparams == 0) then ! Initialize symbol table
            params(1)%symbol = 'PI'
            params(1)%value = (3.14159265358979_r8, 0.0_r8)
            params(2)%symbol = 'I'
            params(2)%value = (0.0_r8, 1.0_r8)
            nparams = 2
        end if

        if (len_trim(expr) == 0) then ! Expression empty
            evalerr = 1
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
            evalerr = 2
            write (*, *) 'Error in the evaluation of the expression ', trim(expr)
            write (*, *) "Parentheses don't match"
            write (*, *)
            return
        end if

!*****************************************************************************
! STEP 2: Evaluate token string in postfix order
!*****************************************************************************

        itop = 0
        i = 0
        do while (i < ntok)
            i = i + 1
            x = token(i)
            select case (x%type)
            case ('E') ! Token is end token
                if (itop > 1) then
                    evalerr = 12
                    write (*, *) 'Error: missing operator in expression ', trim(expr)
                    write (*, *)
                    return
                end if
                call popval(val) ! Final result left on stack of values
                exit
            case ('S') ! Token is operand
                call valuep(x%char, cval) ! Evaluate operand
                if (evalerr /= 0) return
                call pushval(cval) ! Put value of operand on stack
            case ('B') ! Token is a binary operator
                if (itop < 2) then
                    evalerr = 5
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
                        evalerr = 10
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
                    evalerr = 6
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
                    evalerr = 7
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
                        evalerr = 14
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
                        evalerr = 9
                        write (*, *) 'Warning: square root of negative real number', &
                            ' in expression ', trim(expr)
                        write (*, *)
                    end if
                    cval = sqrt(oper1)
                case ('ABS')
                    cval = abs(oper1)
                case ('LOG')
                    if (real(oper1, r8) <= 0. .and. aimag(oper1) == 0.) then
                        evalerr = 8
                        write (*, *) 'Error: negative real or zero argument for', &
                            ' natural logarithm in expression ', trim(expr)
                        write (*, *)
                        return
                    end if
                    cval = log(oper1)
                case ('LOG10')
                    if (real(oper1, r8) <= 0. .and. aimag(oper1) == 0.) then
                        evalerr = 8
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
                        evalerr = 7
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
                    evalerr = 13
                    write (*, *) 'Error: the function ', trim(x%char), ' is undefined', &
                        ' in the expression ', trim(expr)
                    write (*, *)
                    return
                end select
                call pushval(cval) ! Put result back on stack
            end select
        end do

    end subroutine

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

    subroutine evalexpr_c4(expr, val) ! Evaluate expression expr for
        ! val single precision complex
        character(*) :: expr
        complex(c4) :: val
        complex(c8) :: vald

        call evalexpr_c8(expr, vald)
        val = vald
    end subroutine

    subroutine evalexpr_r4(expr, val) ! Evaluate expression expr for
        ! val single precision real
        character(*) :: expr
        real(r4) :: val
        complex(c8) :: vald

        call evalexpr_c8(expr, vald)
        val = real(vald)
    end subroutine

    subroutine evalexpr_r8(expr, val) ! Evaluate expression expr for
        ! val double precision real
        character(*) :: expr
        real(r8) :: val
        complex(c8) :: vald

        call evalexpr_c8(expr, vald)
        val = real(vald, r8)
    end subroutine

    subroutine evalexpr_i4(expr, ival) ! Evaluate expression expr for
        ! ival single precision integer
        character(*) :: expr
        integer(i4) :: ival
        complex(c8) :: vald

        call evalexpr_c8(expr, vald)
        ival = nint(real(vald, r8), i4)
    end subroutine

    subroutine evalexpr_i8(expr, ival) ! Evaluate expression expr for
        ! ival double precision integer
        character(*) :: expr
        integer(i8) :: ival
        complex(c8) :: vald

        call evalexpr_c8(expr, vald)
        ival = nint(real(vald, r8), i8)
    end subroutine
    
    !> @brief Returns .true. if ch is a letter and .false. otherwise
    !! param[in] str character input character
    !! @returns logical @n@n .true. if the character is a letter, .false. otherwise.
    pure function is_letter(ch) result(res)
        character, intent(in)   :: ch
        logical :: res

        select case (ch)
        case ('A':'Z', 'a':'z')
            res = .true.
        case default
            res = .false.
        end select
    end function

    subroutine valdef_c8(sym, val) ! Associates sym with val in symbol table,
        ! val double precision complex
        character(*), intent(in) :: sym
        complex(c8), intent(in) :: val
        character(len_trim(sym)) :: usym
        integer :: i
        evalerr = 0
        if (nparams == 0) then ! Initialize symbol table
            params(1)%symbol = 'PI'
            params(1)%value = (3.14159265358979_r8, 0.0_r8)
            params(2)%symbol = 'I'
            params(2)%value = (0.0_r8, 1.0_r8)
            nparams = 2
        end if

! Assign val to sym if sym is already in symbol table
        usym = uppercase(sym)
        if (.not. is_letter(sym(1:1)) .or. len_trim(sym) > 24) then
            evalerr = 11
            write (*, *) 'Error: symbol ', trim(sym), ' has improper format'
            write (*, *)
            return
        end if
        do i = 1, nparams
            if (trim(usym) == trim(params(i)%symbol)) then
                params(i)%value = val
                return
            end if
        end do

        nparams = nparams + 1 ! Otherwise assign val to new symbol sym
        params(nparams)%symbol = usym
        params(nparams)%value = val
    end subroutine

    subroutine valdef_c4(sym, val) ! Associates sym with val in symbol table,
        ! val single precision complex
        character(*), intent(in) :: sym
        complex(c4), intent(in) :: val
        complex(c8) :: vald

        vald = val
        call valdef_c8(sym, vald)
    end subroutine

    subroutine valdef_r8(sym, val) ! Associates sym with val in symbol table,
        ! val double precision real
        character(*), intent(in) :: sym
        real(r8), intent(in) :: val
        complex(c8) :: vald

        vald = cmplx(val, 0.0_r8, c8)
        call valdef_c8(sym, vald)
    end subroutine

    subroutine valdef_r4(sym, val) ! Associates sym with val in symbol table,
        ! val single precision real
        character(*), intent(in) :: sym
        real(r4), intent(in) :: val
        complex(c8) :: vald

        vald = cmplx(val, 0.0, c8)
        call valdef_c8(sym, vald)
    end subroutine

    subroutine valdef_i8(sym, ival) ! Associates sym with ival in symbol table,
        ! ival double precision integer
        character(*), intent(in) :: sym
        integer(i8), intent(in) :: ival
        complex(c8) :: vald

        vald = cmplx(real(ival, r8), 0.0_r8, c8)
        call valdef_c8(sym, vald)
    end subroutine

    subroutine valdef_i4(sym, ival) ! Associates sym with ival in symbol table,
        ! ival single precision integer
        character(*), intent(in) :: sym
        integer(i4), intent(in) :: ival
        complex(c8) :: vald

        vald = cmplx(real(ival, r8), 0.0, c8)
        call valdef_c8(sym, vald)
    end subroutine

    subroutine valdef_char(sym, expr) ! Associates sym with the value of the
        ! expression expr

        character(*), intent(in) :: sym, expr
        complex(c8) :: val

        if (nparams == 0) then ! Initialize symbol table
            params(1)%symbol = 'PI'
            params(1)%value = (3.14159265358979_r8, 0.0_r8)
            params(2)%symbol = 'I'
            params(2)%value = (0.0_r8, 1.0_r8)
            nparams = 2
        end if

        call evalexpr_c8(expr, val) ! val is value of expression expr
        if (evalerr == 0 .or. evalerr == 9) then
            call valdef_c8(sym, val) ! Assign val to symbol sym
        end if
    end subroutine

    subroutine valuep(xinchar, cval) ! Finds double precision complex value
        ! corresponding to number string xinchar
        ! or value in symbol table corresponding
        ! to symbol name xinchar.

        character(*):: xinchar
        complex(c8) :: cval
        real(r8) :: rval
        integer :: ios
        evalerr = 0

        if (is_letter(xinchar(1:1))) then ! xinchar is a symbol
            call getparam(xinchar, cval)
        else ! xinchar is a number string
            call value(xinchar, rval, ios) ! rval is the value of xinchar
            if (ios > 0) then
                evalerr = 3
                write (*, *) 'Error: number string ', trim(xinchar), ' does not correspond to a valid number'
                write (*, *)
            end if
            cval = cmplx(rval, 0.0_r8, c8)
            return
        end if
    end subroutine

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

    subroutine getparam_c8(sym, var) ! Find double precision complex value var
        ! corresponding to symbol sym

        character(*) :: sym
        character(len_trim(sym)) :: usym
        complex(c8) :: var
        integer :: ifind, j
        evalerr = 0
        sym = adjustl(sym)
        if (.not. is_letter(sym(1:1)) .or. len_trim(sym) > 24) then
            evalerr = 11
            write (*, *) 'Error: symbol ', trim(sym), ' has incorrect format'
            write (*, *)
            return
        end if
        ifind = 0
        usym = uppercase(sym)
        do j = 1, nparams
            if (trim(usym) == trim(params(j)%symbol)) then
                var = params(j)%value
                ifind = j
                exit
            end if
        end do
        if (ifind == 0) then
            evalerr = 4
            write (*, *) 'Error: symbol ', trim(sym), ' not in symbol table'
            write (*, *)
            return
        end if
    end subroutine

    subroutine getparam_c4(sym, var) ! Find single precision complex value var
        ! corresponding to symbol sym

        character(*) :: sym
        complex(c4) :: var
        complex(c8) :: vard

        call getparam_c8(sym, vard)
        var = vard
    end subroutine

    subroutine getparam_r8(sym, var) ! Find double precision real value var
        ! corresponding to symbol sym

        character(*) :: sym
        real(r8) :: var
        complex(c8) :: vard

        call getparam_c8(sym, vard)
        var = real(vard, r8)
    end subroutine

    subroutine getparam_r4(sym, var) ! Find single precision real value var
        ! corresponding to symbol sym

        character(*) :: sym
        real(r4) :: var
        complex(c8) :: vard

        call getparam_c8(sym, vard)
        var = real(vard)
    end subroutine

    subroutine getparam_i8(sym, ivar) ! Find double precision integer value ivar
        ! corresponding to symbol sym

        character(*) :: sym
        integer(i8) :: ivar
        complex(c8) :: vard

        call getparam_c8(sym, vard)
        ivar = nint(real(vard, r8), i8)
    end subroutine

    subroutine getparam_i4(sym, ivar) ! Find single precision integer value ivar
        ! corresponding to symbol sym

        character(*) :: sym
        integer(i4) :: ivar
        complex(c8) :: vard

        call getparam_c8(sym, vard)
        ivar = nint(real(vard, r8), i4)
    end subroutine

    subroutine listvar() ! List all variables and their values
        integer :: i
        write (*, '(/a)') ' VARIABLE LIST:'
        if (nparams == 0) then ! Initialize symbol table
            params(1)%symbol = 'PI'
            params(1)%value = (3.14159265358979_r8, 0.0_r8)
            params(2)%symbol = 'I'
            params(2)%value = (0.0_r8, 1.0_r8)
            nparams = 2
        end if
        do i = 1, nparams
            write (*, *) trim(params(i)%symbol), ' = ', params(i)%value
        end do
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
    
    !> @brief convert string to upper case
    !! param[in] str character(*) input string
    !! @returns character(:), allocatable @n@n A string with uppercase characters.
    pure function uppercase(str) result(ucstr)
        character(*), intent(in) :: str
        character(len_trim(str)) :: ucstr
        !private
        integer :: ilen, ioffset, iquote, iqc, iav, i

        ilen = len_trim(str)
        ioffset = iachar('A') - iachar('a')
        iquote = 0
        ucstr = str
        do i = 1, ilen
            iav = iachar(str(i:i))
            if (iquote == 0 .and. (iav == 34 .or. iav == 39)) then
                iquote = 1
                iqc = iav
                cycle
            end if
            if (iquote == 1 .and. iav == iqc) then
                iquote = 0
                cycle
            end if
            if (iquote == 1) cycle
            if (iav >= iachar('a') .and. iav <= iachar('z')) then
                ucstr(i:i) = achar(iav + ioffset)
            else
                ucstr(i:i) = str(i:i)
            end if
        end do
    end function
end module
