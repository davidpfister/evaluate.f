module evaluate_value
    use evaluate_kinds

    implicit none; private

    public :: value

    !> @brief Generic operator for converting a string to a
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

    !> @brief Converts number string to a double
    !! precision real number
    !! param[in] str character(*) input string
    !! param[out] rnum real(r8) output value
    !! param[out] ios integer error code
    subroutine value_r8(str, rnum, ios)
        character(*), intent(in)    :: str
        real(r8), intent(out)       :: rnum
        integer, intent(out)        :: ios
        !private
        integer :: ilen, ipos

        ilen = len_trim(str)
        ipos = scan(str, 'Ee')
        if (.not. is_digit(str(ilen:ilen)) .and. ipos /= 0) then
            ios = 3
            return
        end if
        read (str, *, iostat=ios) rnum
    end subroutine

    !> @brief Converts number string to a
    !! single precision real number
    !! param[in] str character(*) input string
    !! param[out] rnum real(r4) output value
    !! param[out] ios integer error code
    subroutine value_r4(str, rnum, ios)
        character(*), intent(in)    :: str
        real(r4), intent(out)       :: rnum
        integer, intent(out)        :: ios
        !private
        real(r8)     :: rnumd

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
    !! param[in] str character(*) input string
    !! param[out] inum integer(i8) output value
    !! param[out] ios integer error code
    subroutine value_i8(str, inum, ios)
        character(*), intent(in)    :: str
        integer(i8), intent(out)    :: inum
        integer, intent(out)        :: ios
        !private
        real(r8) :: rnum

        call value_r8(str, rnum, ios)
        if (abs(rnum) > huge(inum)) then
            ios = 15
            return
        end if
        inum = nint(rnum, i8)
    end subroutine

    !> @brief Converts number string to a
    !! single precision integer value
    !! param[in] str character(*) input string
    !! param[out] inum integer(i8) output value
    !! param[out] ios integer error code
    subroutine value_i4(str, inum, ios)
        character(*), intent(in)    :: str
        integer(i4), intent(out)    :: inum
        integer, intent(out)        :: ios
        !private
        real(r8) :: rnum

        call value_r8(str, rnum, ios)
        if (abs(rnum) > huge(inum)) then
            ios = 15
            return
        end if
        inum = nint(rnum, i4)
    end subroutine

    !> @brief Returns .true. if ch is a digit (0,1,...,9) and .false. otherwise
    !! param[in] str character input character
    !! @returns logical @n@n .true. if the character is a digit, .false. otherwise.
    pure function is_digit(ch) result(res)
        character, intent(in) :: ch
        logical :: res

        select case (ch)
        case ('0':'9')
            res = .true.
        case default
            res = .false.
        end select
    end function
end module

