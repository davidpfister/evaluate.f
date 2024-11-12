# evaluate.f

This repo contains routines for evaluating mathematical expressions contained in strings, e.g., "cos((a+b)^2+1.6*log(c))".
These strings expressions can contain numbers (e.g., 1.5 or 1.5e6), previously defined parameters (like a,b,c above), 
arithmetic operators (+,-,*,/,^, **), and any of the functions (cos, sin, sqrt, exp, log, log10, 
sinh, cosh, tanh, acos, asin, atan, abs, ang, real, imag, conjg, complex). 
They can also contain nested levels of parentheses. The module also contains routines for defining and retrieving user-specified parameters. The quantity pi and the imaginary unit i are pre-defined parameters.

This is a modern version of the code developped by [George Benthien](https://gbenthien.net/strings/index.html)

The user can assign values to parameters that can be used in expressions with
the subroutine defparam. The calling syntax is
```
    call defparam(symbol,value) or call defparam(symbol,expr)
```
where symbol is the desired parameter name; value is a real, integer, or
complex variable (single or double precision); and expr is a string
containing an expression to be evaluated. The value obtained by evaluating the
expression expr is associated with the parameter symbol. Parameter names must
begin with a letter (a-z, A-Z) and must not be longer than 24 characters.
Parameter names are not case dependent.

An expression can be evaluated with the subroutine evalexpr. The calling
syntax is
```
         call evalexpr(expr,value)
```
where expr is a string containing the expression to be evaluated; value is the
result (single or double precision real, complex or integer). The
expression can contain the arithmetic operations +, -, *, /, ** or ^ as well as
the functions sin, cos, tan, log, ln, abs, exp, sqrt, real, imag, conjg, and
ang (the function ang calculates the phase angle of its complex argument). The
expression can also contain numerical values and previously defined parameters
Grouping by nested levels of parentheses is also allowed. The parameters pi
and i (imaginary unit) are predefined. Complex numbers can be entered as a+i*b
if the parameter i has not been redefined by the user. Complex numbers can also
be entered using complex(a,b).
Example expression:
```
         conjg(((cos(x) + sqrt(a+i*b))^2+complex(ln(1.6e-4),20))/2)
```
An equation of the form 'symbol = expression' can be evaluated using the
subroutine evaleqn. The calling syntax is
```
         call evaleqn(eqn)
```
where eqn is a string containing the equation. The right-hand-side of the
equation is evaluated and assigned to the symbol given by the left-hand-side.

The value assigned to a symbol can be retrieved using the subroutine getparam.
The calling syntax is
```
         call getparam(sym,value)
```
where sym is a symbol string; value is a numeric variable (any of the six
standard types).

The symbols and their values in the symbol table can be listed using the
subroutine listvar. The variable ierr is always available following a call
to any of the above subroutines and is zero if there were no errors. The
possible nonzero values for ierr are

1.       Expression empty
2.       Parentheses don't match
3.       Number string does not correspond to a valid number
4.       Undefined symbol
5.       Less than two operands for binary operation
6.       No operand for unary plus or minus operators
7.       No argument(s) for function
8.       Zero or negative real argument for logarithm
9.       Negative real argument for square root
10.      Division by zero
11.      Improper symbol format
12.      Missing operator
13.      Undefined function
14.      Argument of tangent function a multiple of pi/2