Command Line Calculator

Run calc.exe from command line. You should get the prompt

>>

Typing in 

>> var=1.57e-6

and hitting Enter assigns the value 1.57e-6 to the variable var. The variables
pi and i (imaginary unit) are predefined. Variable names can be up to 24 
characters in length and must begin with a letter. Variables are not case 
dependent. If you type in an expression such as

>> sqrt(cos(x))

the expression will be evaluated and you will get

>> Result = (<real>,<imag>)

provided there are no errors. Result is now assigned to this value and can be
used in further calculations. Expressions can contain the numeric operators 
+, -, *, /, ^ as well as the functions sin, cos, tan, log, ln, sqrt, exp, 
real, imag, conjg, ang, and abs. The function ang gives the phase angle of its 
complex argument in radians. The function log is the logarithm to the base 10 
and the function ln is the natural logarithm. The expression can also contain 
previously defined variables. Complex numbers can be entered in the form a+i*b
(if i has not been redefined by the user) or complex(a,b). All calculations are
done in double precision complex. If you type in a statement of the form

>> var=<expression>

the expression on the right-hand-side will be evaluated and assigned to the
variable on the left-hand-side. To list all variables and their values type in

>> list

at the prompt.  Hitting return at the prompt 
without entering anything exits the program.
