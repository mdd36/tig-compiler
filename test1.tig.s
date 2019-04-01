L357:
move $t546, $ra
subi $t547, $sp, 0
move $sp, $t547
lw $t550, 0($fp)
lw $t549, 0($t550)
move $a0, $t549
li $t551, 0
move $a1, $t551
la $t552, L355
move $a2, $t552 
jal do_nothing1
move $ra, $t546
j L356
L356:
L359:
move $t553, $ra
subi $t554, $sp, 0
move $sp, $t554
move $a0, $fp
lw $t557, 4($fp)
addi $t556, $t557, 1
move $a1, $t556
jal do_nothing2
move $ra, $t553
j L358
L358:
strL361:
move $t558, $ra
subi $t559, $sp, 0
move $sp, $t559
move $a0, $fp
lw $t561, 4($fp)
move $a1, $t561
la $t562, L354
move $a2, $t562
jal do_nothing1
move $ra, $t558
j L360
L360:
str2