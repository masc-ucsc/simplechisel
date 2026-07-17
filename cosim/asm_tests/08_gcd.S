.globl _start
_start:
    addi x5, x0, 48
    addi x6, x0, 18
loop:
    beq  x5, x6, done
    bge  x5, x6, a_ge_b
    sub  x6, x6, x5
    j    loop
a_ge_b:
    sub  x5, x5, x6
    j    loop
done:
    add  x10, x5, x0
    j    .
