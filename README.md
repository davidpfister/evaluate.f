<a id="readme-top"></a>

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <h3 align="center">Evaluate.f</h3>

  <p align="center">
    A fast equation parser in Fortran
    <br />
    <a href="https://github.com/davidpfister/evaluate.f"><strong>Explore the project »</strong></a>
    <br />
  </p>
</div>



<!-- TABLE OF CONTENTS -->
[TOC]

# Introduction
<!-- ABOUT THE PROJECT -->
## About the Project
<p align="center">
  <img src="https://github.com/davidpfister/evaluate.f/blob/master/.dox/images/calc1.jpg?raw=true">
</p>

This repo contains routines for evaluating mathematical expressions contained in strings, e.g., `cos((a+b)^2+1.6*log(c))`. 
It is a modernized version of the code developed by [George Benthien](https://gbenthien.net/strings/index.html). 

I came across George's code while working on [benchmark.f](https://github.com/davidpfister/benchmark.f). I created an example which contains all the equation parsers I could find and I was impressed by the 
performances of the present code. I decided to give it a bit more visibility by creating this repo and
started to work on some modernization. It is now up-to-date, fully type-explicit, and fpm-compatible. 

The strings expressions can contain numbers (e.g., `1.5` or `1.5e6`), 
previously defined parameters (like `a`,`b`, `c` above), arithmetic operators (`+`,`-`,`*`,`/`,`^`, `**`), and any of 
the functions (`sin`, `cos`, `tan`, `log10`, `log`, `abs`, `exp`, `sqrt`, 
`real`, `imag`, `conjg`, and `ang`). They can also contain nested levels of parentheses. The module also contains routines for defining and retrieving user-specified parameters. 
The quantity `pi` and the imaginary unit `i` are pre-defined parameters.

* [![fpm][fpm]][fpm-url]
* [![ifort][ifort]][ifort-url]
* [![gfortran][gfortran]][gfortran-url]

<!-- GETTING STARTED -->
## Getting Started

### Requirements

To build that library you need

- a Fortran 2008 compliant compiler, or better, a Fortran 2018 compliant compiler.

The following compilers are tested on the default branch of _evaluate.f_:

<center>

| Name |	Version	| Platform	| Architecture |
|:--:|:--:|:--:|:--:|
| GCC Fortran (MinGW) | 14 | Windows 10 | x86_64 |
| Intel oneAPI classic	| 2021.5	| Windows 10 |	x86_64 |

</center>

Unit test rely on the the header file [`assertion.inc`](https://github.com/davidpfister/fortiche/tree/master/src/assertion). 
Since the whole framework fits in a single file, it has been added directly to the repo. 

Linting, indentation, and styling is done with [fprettify](https://github.com/fortran-lang/fprettify) with the following settings
```bash
fprettify .\src\ -r --case 1 1 1 1 -i 4 --strict-indent --enable-replacements --strip-comments --c-relations
```

<!-- USAGE EXAMPLES -->
## Usage

The user can assign values to parameters that can be used in expressions with
the subroutine `defparam`. The calling syntax is
```fortran
    call defparam(symbol, value) 
    !or
    call defparam(symbol, expr)
```
where _symbol_ is the desired parameter name; _value_ is a _real_, _integer_, or
_complex_ variable (single or double precision); and _expr_ is a string
containing an expression to be evaluated. The value obtained by evaluating the
expression _expr_ is associated with the parameter _symbol_. Parameter names must
begin with a letter (a-z, A-Z) and must not be longer than 24 characters.
Parameter names are not case dependent.

An expression can be evaluated with the subroutine `eval`. The calling
syntax is
```fortran
         call eval(expr,value)
```
where _expr_ is a string containing the expression to be evaluated; value is the
result (single or double precision real, complex or integer). The
expression can contain the arithmetic operations `+`, `-`, `*`, `/`, `**` or `^` 
as well as the functions `sin`, `cos`, `tan`, `log10`, `log`, `abs`, `exp`, `sqrt`, 
`real`, `imag`, `conjg`, and `ang` (the function ang calculates the phase angle 
of its complex argument). The expression can also contain numerical values and 
previously defined parameters

Grouping by nested levels of parentheses is also allowed. The parameters `pi`
and `i` (imaginary unit) are predefined. Complex numbers can be entered as `a+i*b`
if the parameter `i` has not been redefined by the user. Complex numbers can also
be entered using `complex(a,b)`.

Example expression:
```txt
         conjg(((cos(x) + sqrt(a+i*b))^2+complex(ln(1.6e-4),20))/2)
```
The value assigned to a symbol can be retrieved using the subroutine `getparam`.
The calling syntax is:
```fortran
         call getparam(sym,value)
```
where _sym_ is a symbol string; _value_ is a numeric variable (any of the six
standard types).

The symbols and their values in the symbol table can be listed using the
subroutine `listvar`. The variable _ierr_ is always available following 
a call to any of the above subroutines and is zero if there were no errors. 
The possible nonzero values for _ierr_ are:
|Value|Definition|
|:--:|:--|
|1|Expression empty|
2|Parentheses don't match|
3|Number string does not correspond to a valid number|
4|Undefined symbol|
5|Less than two operands for binary operation|
6|No operand for unary plus or minus operators|
7|No argument(s) for function|
8|Zero or negative real argument for logarithm|
9|Negative real argument for square root|
|10|Division by zero|
|11|Improper symbol format|
|12|Missing operator|
|13|Undefined function|
|14|Argument of tangent function a multiple of pi/2|

### Installation

#### Get the code
```bash
git clone https://github.com/davidpfister/evaluate.f
cd evaluate.f
```

#### Build with fpm

The repo is compatible with fpm projects. It can be build using _fpm_
```bash
fpm build
```
For convenience, the  repo also contains a response file that can be invoked as follows: 
```bash
fpm @build
```
(For the Windows users, that command does not work in Powershell since '@' is a reserved symbol. One should
use the '--%' as follows: `fpm --% @build`.
This is linked to the following [issue](https://github.com/urbanjost/M_CLI2/issues/19))

Building with ifort requires to specify the compiler name (gfortran by default)
```bash
fpm @build --compiler ifort
```
Alternatively, the compiler can be set using fpm environment variables.
```bash
set FPM_FC=ifort
```

Besides the build command, several commands are also available:
```bash
@pretiffy
option clean --all
system fprettify .\src\ -r --case 1 1 1 1 -i 4 --strict-indent --enable-replacements --strip-comments --c-relations
system fprettify .\tests\ -r --case 1 1 1 1 -i 4 --strict-indent --enable-replacements --strip-comments --c-relations
system fprettify .\examples\calc\ -r --case 1 1 1 1 -i 4 --strict-indent --enable-replacements --strip-comments --c-relations

@clean
option clean --all

@rebuild
system rmdir /s /q build
option build

@build
option build

@test
options test '-D_QUIET' 

@doc
option clean --all
system cd ./.dox & doxygen ./Doxyfile.in & cd ..
```

The settings to the cpp preprocessor are specified in the file. 

```toml
[preprocess]
cpp.suffixes = ["F90", "f90"]
cpp.macros = ["_FPM"]
```
The `_FPM` macro is used to differentiate the build when compiling with _fpm_ or _Visual Studio_. 
This is mostly present to adapt the hard coded paths that differs in both cases.

#### Build with Visual Studio 2019

The project was originally developed on Windows with Visual Studio 2019. 
The repo contains the solution file (_Evaluate.sln_) to get you started with Visual Studio 2019. 

<!-- CONTRIBUTING -->
### Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. 
Any contributions you make are **greatly appreciated**. So, thank you for considering contributing to _evaluate.f_.
Please review and follow these guidelines to make the contribution process simple and effective for all involved. 
In return, the developers will help address your problem, evaluate changes, and guide you through your pull requests.

By contributing to _evaluate.f_, you certify that you own or are allowed to share the content of your 
contribution under the same license.

### Style

Please follow the style used in this repository for any Fortran code that you contribute. This allows 
focusing on substance rather than style.

### Reporting a bug

A bug is a *demonstrable problem* caused by the code in this repository.
Good bug reports are extremely valuable to us—thank you!

Before opening a bug report:

1. Check if the issue has already been reported
   ([issues](https://github.com/davidpfister/evaluate.f/issues)).
2. Check if it is still an issue or it has been fixed?
   Try to reproduce it with the latest version from the default branch.
3. Isolate the problem and create a minimal test case.

A good bug report should include all information needed to reproduce the bug.
Please be as detailed as possible:

1. Which version of _evaluate.f_ are you using? Please be specific.
2. What are the steps to reproduce the issue?
3. What is the expected outcome?
4. What happens instead?

This information will help the developers diagnose the issue quickly and with
minimal back-and-forth.

### Pull request

If you have a suggestion that would make this project better, please create a pull request. 
You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!
1. Open a [new issue](https://github.com/davidpfister/evaluate.f/issues/new) to
   describe a bug or propose a new feature.
   Refer to the earlier sections on how to write a good bug report or feature    request.
2. Discuss with the developers and reach consensus about what should be done about the bug or 
feature request.
   **When actively working on code towards a PR, please assign yourself to the
   issue on GitHub.**
   This is good collaborative practice to avoid duplicated effort and also inform others what you 
   are currently working on.
3. Create your Feature Branch (```git checkout -b feature/AmazingFeature```)
4. Commit your Changes (```git commit -m 'Add some AmazingFeature'```)
5. Push to the Branch (```git push origin feature/AmazingFeature```)
6. Open a Pull Request with your contribution.
   The body of the PR should at least include a bullet-point summary of the
   changes, and a detailed description is encouraged.
   If the PR completely addresses the issue you opened in step 1, include in
   the PR description the following line: ```Fixes #<issue-number>```. If your PR implements a 
   feature that adds or changes the behavior of _evaluate.f_,
   your PR must also include appropriate changes to the documentation and associated units tests.

In brief, 
* A PR should implement *only one* feature or bug fix.
* Do not commit changes to files that are irrelevant to your feature or bug fix.
* Smaller PRs are better than large PRs, and will lead to a shorter review and
  merge cycle
* Add tests for your feature or bug fix to be sure that it stays functional and useful
* Be open to constructive criticism and requests for improving your code.


<!-- LICENSE -->
## License

Distributed under the MIT License.

<!-- MARKDOWN LINKS & IMAGES -->
[contributors-shield]: https://img.shields.io/github/contributors/davidpfister/evaluate.f.svg?style=for-the-badge
[contributors-url]: https://github.com/davidpfister/evaluate.f/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/davidpfister/evaluate.f.svg?style=for-the-badge
[forks-url]: https://github.com/davidpfister/evaluate.f/network/members
[stars-shield]: https://img.shields.io/github/stars/davidpfister/evaluate.f.svg?style=for-the-badge
[stars-url]: https://github.com/davidpfister/evaluate.f/stargazers
[issues-shield]: https://img.shields.io/github/issues/davidpfister/evaluate.f.svg?style=for-the-badge
[issues-url]: https://github.com/davidpfister/evaluate.f/issues
[license-shield]: https://img.shields.io/github/license/davidpfister/evaluate.f.svg?style=for-the-badge
[license-url]: https://github.com/davidpfister/evaluate.f/master/LICENSE
[gfortran]: https://img.shields.io/badge/gfortran-000000?style=for-the-badge&logo=gnu&logoColor=white
[gfortran-url]: https://gcc.gnu.org/wiki/GFortran
[ifort]: https://img.shields.io/badge/ifort-000000?style=for-the-badge&logo=Intel&logoColor=61DAFB
[ifort-url]: https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler.html
[fpm]: https://img.shields.io/badge/fpm-000000?style=for-the-badge&logo=Fortran&logoColor=734F96
[fpm-url]: https://fpm.fortran-lang.org/