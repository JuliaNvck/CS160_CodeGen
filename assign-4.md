# Assignment 4: Codegen

__RELEASED:__ Thursday, Nov 13
__DUE:__ Wednesday, Nov 26
__LATE DEADLINE 1:__ Wednesday, Dec 3 (-11%)
__LATE DEADLINE 2:__ Friday, Dec 12 (-31%)

You may work in groups of 1--3 people. If working in a group of 2--3, be sure to indicate that when submitting to Gradescope.

## Description

Implement code generation from LIR to x86-64 assembly. The LIR programs are guaranteed to be valid. The resulting assembly must match the output of the reference implementation. The document `x64-info.md` is posted with this assignment and covers the relevant x86-64 assembly information, including all necessary assembly instructions. If you find it useful, you can also consult the following resources (though it shouldn't be necessary):

- https://en.wikibooks.org/wiki/X86_Assembly
- https://www.felixcloutier.com/x86/

Note that x86 assembly has different syntax versions; we are using AT&T syntax (i.e., the syntax of gas, the gnu assembler). If you look up x86 information online, be sure that you're using the right syntax.

## Input/Output Specifications

The input will be a file containing a JSON representation of a valid LIR program. You may use the same `json.cpp` JSON library to parse the input as you used in assign-2 and assign-3.

The output should be the corresponding x86-64 code, in the same format as given by the reference `cflat` implementation (as compared using `diff -wB`).

## Grading

The grading will be done using a set of test suites, weighted evenly. The test suites for this assignment are:

- `TS1`: progam contains only the `main` function with int-typed locals
- `TS2`: TS1 + programs can contain calls and externs/functions besides `main`, but only with <= 6 parameters (everything int-typed)
- `TS3`: TS2 + externs/functions can have > 6 parameters
- `TS4`: TS1 + locals can be arrays or pointers (including pointers to structs)
- `TS5`: TS2 + locals and parameters can be arrays or pointers (including pointers to structs or functions)
- `TS6`: TS3 + locals and parameters can be arrays or pointers (including pointers to structs or functions)

## Reference Solution

I have placed an executable of my own Cflat compiler implementation on CSIL in `~benh/160/cflat`. Use `cflat --help` to see how to use it. In particular for this assignment:

- You can use `cflat` to translate a `*.cb` or `*.lirj` file into x86-64 assembly using `cflat -o asm <file>.{cb, lirj}`.

- You can recover the LIR from a `*.lirj` file using `cflat -o lir <file>.lirj` to help you see what the program is doing.

You can use the reference solution to test your code before submitting. If you have any questions about the output format or the behavior of the implementation you can answer them using the reference solution; this is the solution that Gradescope will use to test your submissions.

## Submitting to Gradescope

The autograder is running Ubuntu 22.04 and has the latest `build-essential` package installed (including `g++` version 11.4.0 and the `make` utility). You may not use any other libraries or packages for your code. Your submission must meet the following requirements:

- There must be a `makefile` file s.t. invoking the `make` command builds your code, resulting in an executable called `codegen`. The executable must take a single argument (the file to run the lowerer on) and produce its output on standard out.

- If your solution contains sub-directories then the entire submission must be given as a zip file (this is required by Gradescope for submissions that contain a directory structure, otherwise it will automatically flatten everything into a single directory). The `makefile` should be in the root directory of the solution.

- The submitted files are not allowed to contain any binaries, and your solution is not allowed to use the network.

## Use AddressSanitizer

Student programs can often contain memory errors that happen not to manifest themselves when the students execute their code locally, but do manifest themselves when the program is uploaded and executed by Gradescope. This difference in behavior can result in confusion because Gradescope is claiming there is an error, but there is seemingly no error when the students run the program themselves (note that there actually is an error, it just happens to not be caught at runtime). To avoid this issue, please always compile your code using AddressSanitizer, which will catch most such errors locally. See the following site for an explanation of how to do so (the given flags are for the `clang` compiler, but they should be the same for `gcc`): https://github.com/google/sanitizers/wiki/AddressSanitizer.
