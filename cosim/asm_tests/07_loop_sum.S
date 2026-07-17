.globl _start
_start:
    addi x5, x0, 0
    addi x6, x0, 1
    addi x7, x0, 5
loop:
    add  x5, x5, x6
    addi x6, x6, 1
    bge  x7, x6, loop
    j    .
