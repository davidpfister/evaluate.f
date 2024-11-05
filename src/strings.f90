module evaluate_strings
    use evaluate_kinds

    implicit none; private

    public :: value, &
              parse, &
              compact, &
              removesp, &
              uppercase, &
              is_letter

    !> @brief Generic operator for converting a number string to a
    !! number. Calling syntax is 'call value(numstring,number,ios)'
    !! where 'numstring' is a number string and 'number' is a
    !! real number or an integer (single or double precision).
    interface value 
        module procedure value_r8
        module procedure value_r4
        module procedure value_i8
        module procedure value_i4
    end interface

contains

    !> @brief Parses the string 'str' into arguments args(1), ..., args(nargs) based on
    !! the delimiters contained in the string 'delims'. Preceding a delimiter in
    !! 'str' by a backslash (\) makes this particular instance not a delimiter.
    !! The integer output variable nargs contains the number of arguments found.
    subroutine parse(str, delims, args, nargs)
        character(*) :: str, delims
        character(len_trim(str)) :: strsav
        character(*), dimension(:) :: args
        !private
        integer :: na, i, nargs, lenstr, k, isp, ich

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

    !> @brief Converts multiple spaces and tabs to single spaces; deletes control characters;
    !! removes initial spaces.
    subroutine compact(str)
        character(*):: str
        character(1):: ch
        character(len_trim(str)):: outstr
        !private 
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

    !> @brief Removes spaces, tabs, control characters in 
    !! string str and replace '**' by '^'
    subroutine removesp(str)
        character(*) :: str
        character(1) :: ch
        character(len_trim(str)) :: outstr
        !private 
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
        if (scan(str, '**') > 0) then
            str = replacesp(outstr, '**', '^')
        end if
    end subroutine

    pure recursive function replacesp(string, search, substitute) result(modifiedString)
        character(*), intent(in)  :: string, search, substitute
        character(:), allocatable :: modifiedString
        !private
        integer :: i, stringLen, searchLen

        stringLen = len(string)
        searchLen = len(search)
        if (stringLen == 0 .or. searchLen == 0) then
            modifiedString = ''
            return
        elseif (stringLen < searchLen) then
            modifiedString = string
            return
        end if
        i = 1
        do
            if (string(i:i + searchLen - 1) == search) then
                modifiedString = string(1:i - 1)//substitute//replacesp(string(i + searchLen:stringLen), search, substitute)
                exit
            end if
            if (i + searchLen > stringLen) then
                modifiedString = string
                exit
            end if
            i = i + 1
            cycle
        end do
    end function

    !> @brief Converts number string to a double 
    !! precision real number
    subroutine value_r8(str, rnum, ios)
        character(*) :: str
        real(r8)     :: rnum
        integer      :: ios
        !private 
        integer :: ilen, ipos

        ilen = len_trim(str)
        ipos = scan(str, 'Ee')
        if (.not. is_i8git(str(ilen:ilen)) .and. ipos /= 0) then
            ios = 3
            return
        end if
        read (str, *, iostat=ios) rnum
    end subroutine

    !> @brief Converts number string to a 
    !! single precision real number
    subroutine value_r4(str, rnum, ios)
        character(*) :: str
        real(r4)     :: rnum
        real(r8)     :: rnumd
        !private
        integer :: ios
        
        call value_r8(str, rnumd, ios)
        if (abs(rnumd) > huge(rnum)) then
            ios = 15
            return
        end if
        if (abs(rnumd) < tiny(rnum)) rnum = 0.0_r4
        rnum = rnumd
    end subroutine

    !> @brief Converts number string to a 
    !! double precision integer value
    subroutine value_i8(str, inum, ios)
        character(*)::str
        integer(i8) :: inum
        real(r8) :: rnum
        !private
        integer :: ios
        
        call value_r8(str, rnum, ios)
        if (abs(rnum) > huge(inum)) then
            ios = 15
            return
        end if
        inum = nint(rnum, i8)
    end subroutine

    !> @brief Converts number string to a 
    !! single precision integer value
    subroutine value_i4(str, inum, ios)
        character(*)::str
        integer(i4) :: inum
        real(r8) :: rnum
        !private
        integer :: ios
        
        call value_r8(str, rnum, ios)
        if (abs(rnum) > huge(inum)) then
            ios = 15
            return
        end if
        inum = nint(rnum, i4)
    end subroutine

    !> @brief convert string to upper case
    function uppercase(str) result(ucstr)
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

    !> @brief convert string to lower case
    function lowercase(str) result(lcstr)
        character(*)             :: str
        character(len_trim(str)) :: lcstr
        !private
        integer :: ilen, ioffset, iquote, i, iav, iqc
        
        ilen = len_trim(str)
        ioffset = iachar('A') - iachar('a')
        iquote = 0
        lcstr = str
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
            if (iav >= iachar('A') .and. iav <= iachar('Z')) then
                lcstr(i:i) = achar(iav - ioffset)
            else
                lcstr(i:i) = str(i:i)
            end if
        end do
    end function

    !> @brief Sets imatch to the position in string of the delimiter matching the delimiter
    !! in position ipos. Allowable delimiters are (), [], {}, <>.
    subroutine match(str, ipos, imatch)
        character(*) :: str
        integer :: ipos
        integer :: imatch
        !private
        character :: delim1, delim2, ch
        integer :: lenstr, idelim1, idelim2, istart, iend, inc, isum, i
        
        lenstr = len_trim(str)
        delim1 = str(ipos:ipos)
        select case (delim1)
        case ('(')
            idelim2 = iachar(delim1) + 1
            istart = ipos + 1
            iend = lenstr
            inc = 1
        case (')')
            idelim2 = iachar(delim1) - 1
            istart = ipos - 1
            iend = 1
            inc = -1
        case ('[', '{', '<')
            idelim2 = iachar(delim1) + 2
            istart = ipos + 1
            iend = lenstr
            inc = 1
        case (']', '}', '>')
            idelim2 = iachar(delim1) - 2
            istart = ipos - 1
            iend = 1
            inc = -1
        case default
            write (*, *) delim1, ' is not a valid delimiter'
            return
        end select
        if (istart < 1 .or. istart > lenstr) then
            write (*, *) delim1, ' has no matching delimiter'
            return
        end if
        delim2 = achar(idelim2) ! matching delimiter

        isum = 1
        do i = istart, iend, inc
            ch = str(i:i)
            if (ch /= delim1 .and. ch /= delim2) cycle
            if (ch == delim1) isum = isum + 1
            if (ch == delim2) isum = isum - 1
            if (isum == 0) exit
        end do
        if (isum /= 0) then
            write (*, *) delim1, ' has no matching delimiter'
            return
        end if
        imatch = i
    end subroutine

    !> @brief Returns .true. if ch is a letter and .false. otherwise
    function is_letter(ch) result(res)
        character :: ch
        logical :: res

        select case (ch)
        case ('A':'Z', 'a':'z')
            res = .true.
        case default
            res = .false.
        end select
    end function

    !> @brief Returns .true. if ch is a digit (0,1,...,9) and .false. otherwise
    function is_i8git(ch) result(res)
        character :: ch
        logical :: res

        select case (ch)
        case ('0':'9')
            res = .true.
        case default
            res = .false.
        end select
    end function

    !> @brief Routine finds the first instance of a character from 'delims' in the
    !! the string 'str'. The characters before the found delimiter are
    !! output in 'before'. The characters after the found delimiter are
    !! output in 'str'. The optional output character 'sep' contains the
    !! found delimiter. A delimiter in 'str' is treated like an ordinary
    !! character if it is preceded by a backslash (\). If the backslash
    !! character is desired in 'str', then precede it with another backslash.
    subroutine split(str, delims, before, sep)
        character(*) :: str, delims, before
        character, optional :: sep
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

    !> @brief Removes backslash (\) characters. Double backslashes (\\) are replaced
    !! by a single backslash.
    subroutine removebksl(str)
        character(*):: str
        !private
        character(1):: ch
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
end module

