#include "../include/assertion.inc"
TESTPROGRAM(main)

TEST(evaluate_tests)
use test_data
use evaluate

character(*), dimension(11), parameter :: var = ['x ', 'y ', 'z ', 'x1', 'x2', &
                                                 'a ', 'b ', 'c ', 'd ', 'e ', 'f ']
double precision :: val
integer :: i

call defparam('x', 0.175_r8)
call defparam('y', 0.110_r8)
call defparam('z', 0.900_r8)
call defparam('x1', 0.508_r8)
call defparam('x2', 30.000_r8)
call defparam('a', 0.900_r8)
call defparam('b', 0.100_r8)
call defparam('c', 0.110_r8)
call defparam('d', 0.120_r8)
call defparam('e', 0.120_r8)
call defparam('f', 0.140_r8)

do i = 1, neq
    call evalexpr(eqstring(i), val)
    EXPECT_NEAR(val, results(i), 1.0e-3)
end do
END_TEST

END_TESTPROGRAM
