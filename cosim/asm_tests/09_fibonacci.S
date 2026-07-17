.globl _start
_start:
    addi x5, x0, 0
    addi x6, x0, 1
    addi x7, x0, 10
loop:
    beq  x7, x0, done
    add  x8, x5, x6
    add  x5, x6, x0
    add  x6, x8, x0
    addi x7, x7, -1
    j    loop
done:
    add  x10, x5, x0
    j    .
