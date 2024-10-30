module evaluate_kinds
    use, intrinsic :: iso_fortran_env, only: i1 => int8, &
                                             i2 => int16, &
                                             i4 => int32, &
                                             i8 => int64, &
                                             r4 => real32, &
                                             r8 => real64, &
                                             r16 => real128, &
                                             c4 => real32, &
                                             c8 => real64
    
    implicit none; private
    
    public :: i1, i2, i4, i8, r4, r8, r16, c4, c8

end module
