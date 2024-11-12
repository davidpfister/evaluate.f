!> @defgroup group_string evaluate_string
!! @brief String module
module evaluate_string
    implicit none; private
    
    public :: uppercase, &
              is_letter
    
    contains
    
    !> @ingroup group_string
    !> @brief Check if the character i s a letter
    !! @param[in] str character input character
    !!
    !! @b Examples
    !! ```fortran
    !! character(*), parameter :: input = 'abcd'
    !! integer :: i 
    !! do i = 1, 4
    !!     if (is_letter(input(i:i))) then
    !!         print*, input(i:i)
    !!     end if
    !! end do
    !! ```
    !! @returns logical @n@n .true. if the character is a letter, .false. otherwise.
    !! @b Remarks
    pure function is_letter(ch) result(res)
        character(1), intent(in)   :: ch
        logical :: res

        select case (ch)
        case ('A':'Z', 'a':'z')
            res = .true.
        case default
            res = .false.
        end select
    end function
    
    !> @ingroup group_string
    !> @brief Convert string to upper case
    !! param[in] str character(*) input string
    !!
    !! @b Examples
    !! ```fortran
    !! character(*), parameter :: input = 'test'
    !! character(:), allocatable :: output
    !! output = uppercase(input)
    !! if (output == 'TEST') print*, 'OK'
    !! ```
    !! @returns character(:), allocatable @n@n A string with uppercase characters.
    !! @b Remarks
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