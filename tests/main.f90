#include "../include/assertion.inc"
TESTPROGRAM(main)

    TEST(evaluate_tests)
        use parameters
        use evaluate

        character(*), dimension(11), parameter :: var = ['x ', 'y ', 'z ', 'x1', 'x2', &
        'a ', 'b ', 'c ', 'd ', 'e ', 'f ']
        double precision :: val
        integer :: i

        call defparam(var(1), 0.175_r8)
        call defparam(var(2), 0.110_r8)
        call defparam(var(3), 0.900_r8)
        call defparam(var(4), 0.508_r8)
        call defparam(var(5), 30.000_r8)
        call defparam(var(6), 0.900_r8)
        call defparam(var(7), 0.100_r8)
        call defparam(var(8), 0.110_r8)
        call defparam(var(9), 0.120_r8)
        call defparam(var(10), 0.120_r8)
        call defparam(var(11), 0.140_r8)

        do i = 1, neq
            call evalexpr(eqstring(i), val)
            EXPECT_NEAR(val, results(i), 1.0e-3)
        end do
    END_TEST
    
END_TESTPROGRAM