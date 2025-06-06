!> @defgroup group_string evaluate_string
!! @brief String module
module evaluate_string
    implicit none; private
    
    public :: uppercase, &
              is_letter
    
    contains
    
    !> @brief   Check if the character is a letter
    !! @ingroup group_string
    !! @param[in] ch input character
    !!
    !! @returns 
    !! _logical_. `.true.` if the character is a letter, `.false.` otherwise.
    !!
    !! @b Examples
    !! @code
    !! character(*), parameter :: input = 'abcd'
    !! integer :: i 
    !! do i = 1, 4
    !!     if (is_letter(input(i:i))) then
    !!         print*, input(i:i)
    !!     end if
    !! end do
    !! @endcode
    !!
    !! @b Remarks
    pure function is_letter(ch) result(res)
        character(1), intent(in)   :: ch
        logical :: res

        select case (ch)
        case ('A':'Z')
            res = .true.
		case ('a':'z')
			res = .true.
        case default
            res = .false.
        end select
    end function
    
    !> @brief   Convert string to upper case
    !! @ingroup group_string
    !! param[in] str input string
    !!
    !! @returns _character(*)_. A string with uppercase characters.
    !!
    !! @b Examples
    !! @code
    !! character(*), parameter :: input = 'test'
    !! character(:), allocatable :: output
    !! output = uppercase(input)
    !! if (output == 'TEST') print*, 'OK'
    !! @endcode
    !!
    !! @b Remarks
    pure function uppercase(str) result(res)
        character(*), intent(in) :: str
        character(len_trim(str)) :: res
        !private
        integer :: ilen, ioffset, iquote, iqc, iav, i

        ilen = len_trim(str)
        ioffset = iachar('A') - iachar('a')
        iquote = 0
        res = str
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
                res(i:i) = achar(iav + ioffset)
            else
                res(i:i) = str(i:i)
            end if
        end do
    end function
    
end module