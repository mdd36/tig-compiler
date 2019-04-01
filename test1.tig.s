L93:
move $t239, $ra
subi $t240, $sp, 0
move $sp, $t240
move $a0, $fp
lw $t243, 4($fp)
addi $t242, $t243, 1
move $a1, $t242
jal do_nothing2
move $ra, $t239
j L92
L92:
str
L95:
move $t244, $ra
subi $t245, $sp, 0
move $sp, $t245
move $a0, $fp
lw $t247, 4($fp)
move $a1, $t247
la $t248, L90
move $a2, $t248
jal do_nothing1
move $ra, $t244
j L94
L94:
str2
L97:
move $t249, $ra
subi $t250, $sp, 0
move $sp, $t250
lw $t253, 0($fp)
lw $t252, 0($t253)
move $a0, $t252
li $t254, 0
move $a1, $t254
la $t255, L91
move $a2, $t255
jal do_nothing1
move $ra, $t249
j L96
L96:
