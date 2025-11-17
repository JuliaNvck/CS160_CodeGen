# x86-64 info
## registers

- there are 16 registers: `%rax`, `%rbx`, `%rcx`, `%rdx`, `%rsi`, `%rdi`, `%rsp`, `%rbp`, `%r{8--15}`

- the `r` prefix means it's an 8-byte register, other names can be used to access their lower bits as 32-, 16-, or 8-bit registers (which we'll need for the `set` instruction)

- register characteristics:

    - `%rsp` is the stack pointer

    - `%rbp` is the frame pointer

    - `%rax` holds function return values

    - `%rax` and `%rdx` are used for division

    - `%rdi`, `%rsi`, `%rdx`, `%rcx`, `%r8`, `%r9` are used to pass the first 6 arguments of a call, in that order

    - caller-save registers: `%rax`, `%rcx`, `%rdx`, `%rdi`, `%rsi`, `%rsp`, `%r{8--11}`

    - callee-save registers: `%rbx`, `%rbp`, `%r{12--15}`

- for x86-64 normally the frame pointer is omitted except in special cases; however we'll use it for all calls

## x86-64 instruction operands

- notation:

    - `imm` ("immediate") denotes a constant value
    - `R` denotes a register
    - `[R]` denotes the value inside a register
    - `<x>` denotes the value at memory address x

```
OPERAND | VALUE
   $imm | the constant 'imm'
    imm | <imm>
      R | [R]
 imm(R) | <imm + [R]>
```

- global values (i.e., contained in the `static` section) are denoted as offsets from the program counter with the offset represented by the global's name, e.g., global value `$glob` is denoted `$glob(%rip)`

- note that not all combinations of operands are valid, and which operands are allowed (and which combinations) depend on the instruction

## instructions

- most instructions use a one-character suffix to indicate the size of the operands; some have 0 (like ret) and some can have multiple (e.g., if they are converting from one size to another):

    - `b` = byte (1 byte)
    - `w` = word (2 bytes)
    - `l` = double word (4 bytes)
    - `q` = quadword (8 bytes)

- we'll be using `q` exclusively

- notation:

    - `S` = source operand
    - `D` = destination operand
    - `C` = condition code, one of {`e` (equal), `z` (zero), `ne` (not equal), `nz` (not zero), `s` (negative), `ns` (not negative), `g` (greater), `ge` (greater or equal), `l` (less), `le` (less or equal)}, all for signed comparisons

- one-character suffix instructions (which will always be `q` for us)

```
mov S, D    : move S to D
movabs S, D : move the immediate S to D
lea S, D    : load effective address of S into D
push S      : push S onto stack (adjusts %rsp)
pop D       : pop from stack into D (adjusts %rsp)
inc D       : increment D by 1 in-place
dec D       : decrement D by 1 in-place
neg D       : two's complement negation of D in-place
add S, D    : add S to D
sub S, D    : subtract S from D
imul S, D   : signed multiply D by S
idiv S      : signed divide %rdx:%rax by S; %rax = quotient
shl S, D    : shift D left by S, do not preserve the sign bit (logical shift left).
shr S, D    : shift D right by S, do not extend the sign bit (logical shift right).
cmp S2, S1  : set condition code according to S1 - S2
```

- no character suffix instructions

```
set{C} D     : set lower byte of D to 1 if condition code is {C}
jmp lbl      : jump to lbl
j{C} lbl     : jump if condition code is {C}
call lbl     : push return address to stack and jump to label (adjusts %rsp)
ret          : pop return address from stack and jump there (adjusts %rsp)
cqo          : sign-extend %rax into %rdx (used for division)
cmov{C} S, D : move S to D if the condition code is {C}
```

## operand restrictions

- notation:

    - `R` = register
    - `M` = memory
    - `I` = immediate

- restrictions:

    - for all `D`, `D` cannot be `I`
    - if there are two operands, they cannot both be `M`
    - `mov`: if `S` is `I`, it must be representable by 32 bits
    - `movabs`: `S` must be `I`, `D` must be `R`
    - `lea`: `S` must be `M`, `D` must be `R`
    - `imul`: `D` must be `R`
    - `idiv`: `S` cannot be `I`
    - `cmp`: `S1` cannot be `I`
    - `cmov`: S must be `R` or `M`, `D` must be `R`
    - `shl`: S must be `I` or `%cl` reg.  If S is `I`, it must be representable by 8 bits but only the lowest 6 bits are used.
    - `shr`: S must be `I` or `%cl` reg.  If S is `I`, it must be representable by 8 bits but only the lowest 6 bits are used.

## general x86-64 calling convention

- to call a function:

    - save any used caller-save registers
    - place first 6 integer/pointer args in the argument-passing registers
    - place any remaining args on the stack (first arg topmost)
    - execute `call` (if it has a return value, it will be in `%rax`)
    - pop any args put on the stack
    - restore any saved caller-save registers

- function prologue:

    - if using any callee-save registers, push them onto the stack
    - allocate space on stack for locals (remember that any arguments not passed in registers are on the stack at a positive offset from %rbp)

- function epilogue:

    - place return value (if any) in `%rax`
    - deallocate stack frame
    - restore callee-save registers
    - execute `ret`

- the stack pointer must be double-word aligned (i.e., `%rsp` % 16 = 0) immediately before a `call` and after a `ret`

    - this matters when pushing args onto the stack before a call and when popping them off the stack when the call returns; we may need to push an extra word before the call to keep the stack aligned, and remember to pop the extra word when the call returns

## compilation

- to compile the resulting assembly program, supposing it is contained in `file.s`: 

    - `gcc -g runtime.c file.s`, where `runtime.c` is the cflat runtime library
    
    - `gcc -g -c runtime.c <file>.s` if the program calls extern functions that aren't in the runtime library (this will produce an object file instead of an executable, it will then need to be linked with other object files that contain the extern function definitions to get an executable)

    - the `-g` flag means we can use gdb or lldb to debug the executable
