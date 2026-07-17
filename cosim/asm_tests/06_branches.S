.globl _start
_start:
    addi x5, x0, 3
    addi x6, x0, 3
    addi x7, x0, 0
    beq  x5, x6, equal
    addi x7, x7, 99
equal:
    addi x7, x7, 1
    bne  x5, x6, notequal
    addi x7, x7, 2
notequal:
    addi x8, x0, 1
    blt  x8, x5, less
    addi x7, x7, 99
less:
    addi x7, x7, 4
    bge  x5, x8, done
    addi x7, x7, 99
done:
    j    .
