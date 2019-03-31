L284:
li $t428, 0
sw $t428, 0($fp)
move $t429, $ra
subi $t430, $sp, 0
move $sp, $t430
lw $t433, 0($fp)
lw $t432, 0($t433)
move $a0, $t432
lw $t434, 0($fp)
move $a1, $t434
jal L282
move $ra, $t429
j L283
L283:
L286:
lw $t435, 4($fp)
j L285
L285:
