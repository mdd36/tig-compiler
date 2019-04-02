g:
lw $t385, 0($fp)
move $a0, $t385
move $a1, $t381
move $v0, $t380
j L56
L56:
f:
lw $t386, 0($fp)
move $a0, $t386
move $a1, $t383
move $t387, $ra
move $a0, $fp
move $a1, $t382
jal g
move $ra, $t387
move $v0, $v0
j L57
L57:
main:
lw $t389, 0($fp)
move $a0, $t389
move $t390, $ra
lw $t393, 0($fp)
lw $t392, 0($t393)
move $a0, $t392
li $t394, 3
move $a1, $t394
jal f
move $ra, $t390
move $v0, $v0
j L58
L58:
