# evaluate.f

This repo contains routines for evaluating mathematical expressions contained in strings, e.g., "cos((a+b)^2+1.6*log(c))".
These strings expressions can contain numbers (e.g., 1.5 or 1.5e6), previously defined parameters (like a,b,c above), arithmetic operators (+,-,*,/,^), and any of the functions (cos, sin, sqrt, exp, log, ln, abs, ang, real, imag, conjg, complex). They can also contain nested levels of parentheses. The module also contains routines for defining and retrieving user-specified parameters. The quantity pi and the imaginary unit i are pre-defined parameters.

This is a modern version of the code developped by [George Benthien](https://gbenthien.net/strings/index.html)
