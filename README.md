# FEM

Program solves ODE-s given by set of equations
```
(a(x)u'(x))' + b(x)u'(x) + c(x)u(x) = f(x)
-a(0)u'(0) + k*u(0) = l
u(1) = ur
```
using finite element method with number of elements specified by user.

## Usage

`stack exec fem -- -gui | -cli [<file>.txt]`

Program takes one or two arguments: `-gui|-cli` and optional name of .txt file with any combination of a(x), b(x), c(x), f(x), number of elements to use, k, l and ur specified, each in separate line, like in example: 
```
a(x): 	-1 * x ^ 2 - 1
b(x): 	4 * x + 1
c(x):	-4
f(x): 	2 * x ^ 2 - 4 * x + 3 
n: 	10
k: 	-0.5
l: 	1
ur: 	0
```
When run with only `-cli`, program will ask for input from user. Program returns chart of u(x) in .svg file, named "chart" for input from cmd, "guiPlot" when used with gui or the same as file passed as an argument.

### Dependencies
* **[haskell-chart](https://github.com/timbod7/haskell-chart/wiki)**
* **[QuickCheck](https://github.com/nick8325/quickcheck)**
* **[haskell-gi](https://github.com/haskell-gi/haskell-gi)**
* **[gi-gtk](https://hackage.haskell.org/package/gi-gtk)**
* **[Attoparsec](https://hackage.haskell.org/package/attoparsec)**

### Author
* **[Paweł Kopel](https://github.com/PKopel)**
