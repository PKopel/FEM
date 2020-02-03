# FEM

Program solves ODE-s given by set of equations
```
(a(x)u'(x))' + b(x)u'(x) + c(x)u(x) = f(x)
-a(0)u'(0) + k*u(0) = l
u(1) = ur
```
using finite element method with number of elements specified by user.

## Usage

Program can take as an argument name of .txt file with a(x), b(x), c(x), f(x), number of elements to use, k, l and ur specified, each in separate line, like in example: 
```
x 2 ^ 1 + -1 *
4 x * 1 +
-4
2 x 2 ^ * 3 + 4 x * -
10
-0.5
1
0
```
When run without any arguments, program will ask for input from cmd. Functions should be formated in [RPN](https://en.wikipedia.org/wiki/Reverse_Polish_notation), like in the above example. Program returns chart of u(x) in .svg file, named "chart" for input from cmd or the same as file passed as an argument.

### Dependencies
* **[haskell-chart](https://github.com/timbod7/haskell-chart/wiki)**
* **[QuickCheck](https://github.com/nick8325/quickcheck)**

### Author
* **[Paweł Kopel](https://github.com/PKopel)**
