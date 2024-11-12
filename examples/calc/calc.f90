 program calc ! Command line complex calculator
    use evaluate

    character(len=132) :: str, line, ustr
    character(len=24) :: varname
    complex(c8) :: valcd
    integer :: lstr, ipos, ierr

    write (*, *)
    write (*, *) '   Welcome to the command line complex calculator. '
    write (*, *) '   The program can be exited at any time by hitting'
    write (*, *) "   'Enter' at the prompt without entering any data."
    write (*, *) "   Help information can be obtained by typing 'help' at"
    write (*, *) "   the prompt or by reading the file 'CalcReadMe.txt'."

    do
        write (*, '(/a)', advance='no') ' >> ' ! Command line prompt
        read (*, '(a)') str
        
        ustr = uppercase(str)
        lstr = len_trim(str)
        if (lstr == 0) exit
        associate(str => trim(adjustl(ustr)))
            
            if (str == 'LIST' .or. str == '..') then
                call listvar() ! List all variables and their values
                cycle
            end if
        
            if (str == 'HELP' .or. str == '?') then
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
            
            if (str == 'QUIT') then
                exit
            end if
        end associate
        ipos = index(str, '=')
        if (ipos == 0) then
            call eval(str, valcd, ierr) ! Evaluate expression
            if (ierr == 0 .or. ierr == 9) then
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
    
    !> @brief Converts multiple spaces and tabs to single spaces; 
    !! deletes control characters; removes initial spaces.
    !! param[inout] str character(*) input string
    pure subroutine compact(str)
        character(*), intent(inout) :: str
        !private
        character(1)                :: ch
        character(len_trim(str))    :: outstr
        integer :: i, k, ich, lenstr, isp

        str = adjustl(str)
        lenstr = len_trim(str)
        outstr = ' '
        isp = 0
        k = 0

        do i = 1, lenstr
            ch = str(i:i)
            ich = iachar(ch)

            select case (ich)
            case (9, 32) ! space or tab character
                if (isp == 0) then
                    k = k + 1
                    outstr(k:k) = ' '
                end if
                isp = 1
            case (33:) ! not a space, quote, or control character
                k = k + 1
                outstr(k:k) = ch
                isp = 0
            end select
        end do
        str = adjustl(outstr)
    end subroutine

    !> @brief Evaluate an equation
    !! param[inout] eqn character(*) input equation string 
    subroutine evaleqn(eqn)
        character(*), intent(inout)    :: eqn
        !private
        integer :: nargs
        character(len(eqn)) :: args(2)
        complex(c8) :: val

        call parse(eqn, '=', args, nargs) ! Seperate right- and left-hand-sides
        call defparam(adjustl(args(1)), args(2)) ! Evaluate right-hand-side and
                                                 ! assign to symbol on 
                                                 ! the left-hand-side.
    end subroutine
    
    !> @brief Parses the string 'str' into arguments args(1), ..., args(nargs) based on
    !! the delimiters contained in the string 'delims'. Preceding a delimiter in
    !! 'str' by a backslash (\) makes this particular instance not a delimiter.
    !! The integer output variable nargs contains the number of arguments found.
    !! @param[inout] str character(*) input string
    !! @param[in] delims character(*) input delimiter
    !! @param[inout] args character(*), parsed arguments
    !! @param[out] nargs integer number of arguments
    pure subroutine parse(str, delims, args, nargs)
        character(*), intent(inout)     :: str
        character(*), intent(in)        :: delims
        character(*), intent(inout)     :: args(:)
        integer, intent(out)            :: nargs
        !private
        character(len_trim(str)) :: strsav
        integer :: na, i, lenstr, k, isp, ich

        strsav = str
        call compact(str)
        na = size(args)
        do i = 1, na
            args(i) = ' '
        end do
        nargs = 0
        lenstr = len_trim(str)
        if (lenstr == 0) return
        k = 0

        do
            if (len_trim(str) == 0) exit
            nargs = nargs + 1
            call split(str, delims, args(nargs))
            call removebksl(args(nargs))
        end do
        str = strsav
    end subroutine
    
    !> @brief Removes backslash (\) characters. Double backslashes (\\) are replaced
    !! by a single backslash.
    !! param[inout] str character(*) input string
    pure subroutine removebksl(str)
        character(*), intent(inout)    :: str
        !private
        character(1) :: ch
        integer :: i, k, ibsl, lenstr
        character(len_trim(str))::outstr

        str = adjustl(str)
        lenstr = len_trim(str)
        outstr = ' '
        k = 0
        ibsl = 0 ! backslash initially inactive

        do i = 1, lenstr
            ch = str(i:i)
            if (ibsl == 1) then ! backslash active
                k = k + 1
                outstr(k:k) = ch
                ibsl = 0
                cycle
            end if
            if (ch == '\') then ! backslash with backslash inactive
                ibsl = 1
                cycle
            end if
            k = k + 1
            outstr(k:k) = ch ! non-backslash with backslash inactive
        end do

        str = adjustl(outstr)
    end subroutine
    
    !> @brief Routine finds the first instance of a character from 'delims' in the
    !! the string 'str'. The characters before the found delimiter are
    !! output in 'before'. The characters after the found delimiter are
    !! output in 'str'. The optional output character 'sep' contains the
    !! found delimiter. A delimiter in 'str' is treated like an ordinary
    !! character if it is preceded by a backslash (\). If the backslash
    !! character is desired in 'str', then precede it with another backslash.
    pure subroutine split(str, delims, before, sep)
        character(*), intent(inout)         :: str 
        character(*), intent(in)            :: delims
        character(*), intent(inout)         :: before
        character, intent(inout), optional  :: sep
        !private
        logical :: pres
        character :: ch, cha
        integer :: lenstr, k, ibsl, i, iposa, ipos

        pres = present(sep)
        str = adjustl(str)
        call compact(str)
        lenstr = len_trim(str)
        if (lenstr == 0) return ! string str is empty
        k = 0
        ibsl = 0 ! backslash initially inactive
        before = ' '
        do i = 1, lenstr
            ch = str(i:i)
            if (ibsl == 1) then ! backslash active
                k = k + 1
                before(k:k) = ch
                ibsl = 0
                cycle
            end if
            if (ch == '\') then ! backslash with backslash inactive
                k = k + 1
                before(k:k) = ch
                ibsl = 1
                cycle
            end if
            ipos = index(delims, ch)
            if (ipos == 0) then ! character is not a delimiter
                k = k + 1
                before(k:k) = ch
                cycle
            end if
            if (ch /= ' ') then ! character is a delimiter that is not a space
                str = str(i + 1:)
                if (pres) sep = ch
                exit
            end if
            cha = str(i + 1:i + 1) ! character is a space delimiter
            iposa = index(delims, cha)
            if (iposa > 0) then ! next character is a delimiter
                str = str(i + 2:)
                if (pres) sep = cha
                exit
            else
                str = str(i + 1:)
                if (pres) sep = ch
                exit
            end if
        end do
        if (i >= lenstr) str = ''
        str = adjustl(str) ! remove initial spaces
    end subroutine

end program

