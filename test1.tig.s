main:
move $t370, $ra
lw $t373, 0($fp)
lw $t372, 0($t373)
move $a0, $t372
li $t374, 3
move $a1, $t374
jal f
move $ra, $t370
move $v0, $v0
j L26
L26:
f:
move $t376, $ra
move $a0, $fp
move $a1, $t368
jal g
move $ra, $t376
move $t375, $v0
addi $t378, $t375, 1
move $v0, $t378
j L27
L27:
g:
move $v0, $t367
j L28
L28:
