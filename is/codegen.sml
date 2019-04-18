structure ASM = Assem
structure T = Tree

signature CODEGEN =
sig
    structure Frame : FRAME
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure Mipsgen : CODEGEN =
struct
    exception ArgCount of string
    exception DivBy0 of string
    structure Frame = MipsFrame

    fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
        let
            val ilist = ref []
            fun emit x = ilist := x :: !ilist
            fun result (gen) = let val t = Temp.newtemp() in gen t; t end

            fun removeSquiggle x = if x < 0 then "-" ^ Int.toString (~x) else Int.toString x

            fun numBits x = Math.log10(real(x)) / Math.log10(real(2))

            fun isShiftMult x  = (* Check if the immed for mult can be done as a shift instead *)
                let
                    val bin = numBits x
                    val intBin = real (floor bin)
                    val tol = 0.000000000001 (* Largest delta that still correctly works at 2^32 +/- 1 *)
                in
                    bin - intBin < tol
                end

            fun oper2str(T.MINUS) = "sub"
            |   oper2str(T.PLUS)  = "add"
            |   oper2str(T.MUL)   = "mul"
            |   oper2str(T.DIV)   = "div"

      fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
      | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)),e2)) =
					emit(ASM.OPER{assem = "sw `s1, " ^ removeSquiggle i ^ "(`s0)\n",
								src = [munchExp e1, munchExp e2],
								dst = [],
								jump = NONE})

		  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)),e2)) =
				emit(ASM.OPER{assem = "sw `s1, " ^ removeSquiggle i ^ "(`s0)\n",
							src = [munchExp e1, munchExp e2],
							dst = [],
							jump = NONE})

      | munchStm (T.MOVE(T.MEM(T.BINOP(T.MINUS, e1, T.CONST i)),e2)) =
            emit(ASM.OPER{assem = "sw `s1, " ^ removeSquiggle (~i) ^ "(`s0)\n",
                        src = [munchExp e1, munchExp e2],
                        dst = [],
                        jump = NONE})

      | munchStm (T.MOVE(T.MEM(T.BINOP(T.MINUS, T.CONST i, e1)),e2)) =
          emit(ASM.OPER{assem = "sw `s1, " ^ removeSquiggle (~i) ^ "(`s0)\n",
                      src = [munchExp e1, munchExp e2],
                      dst = [],
                      jump = NONE})

      | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) =
            emit(ASM.OPER{assem = "sw 's0, " ^ removeSquiggle i ^ "($0)\n",
                        src = [munchExp e2],
                        dst = [],
                        jump = NONE})

      | munchStm(T.MOVE(T.MEM(e1), e2)) =
            emit(ASM.OPER{assem="sw `s0, 0(`s1)\n",
            src=[munchExp e2, munchExp e1], dst=[], jump=NONE})

        (* Cut out some moves by looking ahead one level *)
        | munchStm (T.MOVE(T.TEMP e1, T.BINOP(T.PLUS, e2, T.CONST i))) =
              emit(ASM.OPER{assem="addi `d0, `s0, " ^ removeSquiggle i ^ "\n",
              src=[munchExp e2], dst=[e1], jump=NONE})
        | munchStm (T.MOVE(T.TEMP e1, T.BINOP(T.PLUS, T.CONST i, e2))) =
              emit(ASM.OPER{assem="addi `d0, `s0, " ^ removeSquiggle i ^ "\n",
              src=[munchExp e2], dst=[e1], jump=NONE})

        | munchStm (T.MOVE(T.TEMP e1, T.BINOP(T.MINUS, e2, T.CONST i))) =
              emit(ASM.OPER{assem="addi `d0, `s0, " ^ removeSquiggle(~i) ^ "\n",
              src=[munchExp e2], dst=[e1], jump=NONE})
        | munchStm (T.MOVE(T.TEMP e1, T.BINOP(T.MINUS, T.CONST i, e2))) =
              emit(ASM.OPER{assem="addi `d0, `s0, " ^ removeSquiggle(~i) ^ "\n",
              src=[munchExp e2], dst=[e1], jump=NONE})

        | munchStm (T.MOVE(T.TEMP e1, T.BINOP(oper, e2, e3))) =
              emit(ASM.OPER{assem=oper2str oper ^ " `d0, `s0, `s1\n",
              src=[munchExp e2, munchExp e3], dst=[e1], jump=NONE})
        | munchStm (T.MOVE(T.TEMP e1, T.MEM(T.CONST immed))) = 
              emit(ASM.OPER{assem="lw `d0, " ^ removeSquiggle immed ^ "($0)\n",
              src=[], dst=[e1], jump=NONE})
        | munchStm(T.MOVE(T.TEMP e1, T.MEM(T.BINOP(T.PLUS, rs, T.CONST immed)))) =
                emit(ASM.OPER{assem="lw `d0, " ^ removeSquiggle immed ^ "(`s0)\n",
                src=[munchExp rs], dst=[e1], jump=NONE})
        |   munchStm(T.MOVE(T.TEMP dest, T.MEM(T.BINOP(T.MINUS, rs, T.CONST immed)))) =
                emit(ASM.OPER{assem="lw `d0, " ^ removeSquiggle (~immed) ^ "(`s0)\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
        |   munchStm(T.MOVE(T.TEMP dest, T.MEM(T.BINOP(T.PLUS, T.CONST immed, rs)))) =
                emit(ASM.OPER{assem="lw `d0, " ^ removeSquiggle immed ^ "(`s0)\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
        |   munchStm(T.MOVE(T.TEMP dest, T.MEM(T.BINOP(T.MINUS, T.CONST immed, rs)))) =
                emit(ASM.OPER{assem="lw `d0, " ^ removeSquiggle (~immed) ^ "(`s0)\n",
                src=[munchExp rs], dst=[dest], jump=NONE})

        | munchStm (T.MOVE(T.TEMP e1, T.CONST i)) =
              emit(ASM.OPER{assem="li `d0, " ^ removeSquiggle i ^ "\n",
              src=[], dst=[e1], jump=NONE})
        
        | munchStm (T.MOVE(T.TEMP rd, e2)) =
              emit(ASM.MOVE{assem="move `d0, `s0\n",
              src= munchExp e2, dst=rd})

        | munchStm (T.JUMP(T.NAME label, _)) =
              emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME [label]})

			  | munchStm (T.JUMP(e, labs)) =
					emit(ASM.OPER{assem = "jr 's0\n",
					src=[munchExp e], dst=[], jump=SOME labs})
       (* Case where both are constants, help out our branch predictor *)
        | munchStm(T.CJUMP(T.LT, T.CONST i1, T.CONST i2, l1, l2)) =
              if i1 < i2 then emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l1]})
              else emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l2]})
        | munchStm(T.CJUMP(T.LE, T.CONST i1, T.CONST i2, l1, l2)) =
              if i1 <= i2 then emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l1]})
              else emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l2]})
        | munchStm(T.CJUMP(T.GT, T.CONST i1, T.CONST i2, l1, l2)) =
              if i1 > i2 then emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l1]})
              else emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l2]})
        | munchStm(T.CJUMP(T.GE, T.CONST i1, T.CONST i2, l1, l2)) =
              if i1 >= i2 then emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l1]})
              else emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l2]})
        | munchStm(T.CJUMP(T.EQ, T.CONST i1, T.CONST i2, l1, l2)) =
              if i1 = i2 then emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l1]})
              else emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l2]})
        | munchStm(T.CJUMP(T.NE, T.CONST i1, T.CONST i2, l1, l2)) =
              if i1 <> i2 then emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l1]})
              else emit(ASM.OPER{assem="j `j0\n",
              src=[], dst=[], jump=SOME[l2]})
        | munchStm (T.CJUMP(T.LT, e1, T.CONST 0, l1, l2)) =
              emit(ASM.OPER{assem="bltz `s0, `j0\n`j1\n",
              src=[munchExp e1], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.LE, e1, T.CONST 0, l1, l2)) =
              emit(ASM.OPER{assem="blez `s0, `j0\n`j1\n",
              src=[munchExp e1], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.GT, e1, T.CONST 0, l1, l2)) =
              emit(ASM.OPER{assem="bgtz `s0, `j0\n`j1\n",
              src=[munchExp e1], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.GE, e1, T.CONST 0, l1, l2)) =
              emit(ASM.OPER{assem="bgez `s0, `j0\n`j1\n",
              src=[munchExp e1], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.EQ, e1, T.CONST 0, l1, l2)) =
              emit(ASM.OPER{assem="beqz `s0, `j0\n`j1\n",
              src=[munchExp e1], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.NE, e1, T.CONST 0, l1, l2)) =
              emit(ASM.OPER{assem="bnez `s0, `j0\n`j1\n",
              src=[munchExp e1], dst=[], jump=SOME [l1, l2]})

        | munchStm (T.CJUMP(T.LT, e1, e2, l1, l2)) =
              emit(ASM.OPER{assem="blt `s0, `s1, `j0\nj `j1\n",
              src=[munchExp e1, munchExp e2], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.LE, e1, e2, l1, l2)) =
               emit(ASM.OPER{assem="ble `s0, `s1, `j0\nj `j1\n",
              src=[munchExp e1, munchExp e2], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.ULT, e1, e2, l1, l2)) =
              emit(ASM.OPER{assem="bltu `s0, `s1, `j0\nj `j1\n",
              src=[munchExp e1, munchExp e2], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.ULE, e1, e2, l1, l2)) =
              emit(ASM.OPER{assem="bleu `s0, `s1, `j0\nj `j1\n",
              src=[munchExp e1, munchExp e2], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.GT, e1, e2, l1, l2)) =
              emit(ASM.OPER{assem="bgt `s0, `s1, `j0\nj `j1\n",
              src=[munchExp e1, munchExp e2], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.GE, e1, e2, l1, l2)) =
               emit(ASM.OPER{assem="bge `s0, `s1, `j0\nj `j1\n",
              src=[munchExp e1, munchExp e2], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.UGT, e1, e2, l1, l2)) =
              emit(ASM.OPER{assem="bgtu `s0, `s1, `j0\nj `j1\n",
              src=[munchExp e1, munchExp e2], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.UGE, e1, e2, l1, l2)) =
              emit(ASM.OPER{assem="bgeu `s0, `s1, `j0\nj `j1\n",
              src=[munchExp e1, munchExp e2], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.EQ, e1, e2, l1, l2)) =
              emit(ASM.OPER{assem="beq `s0, `s1, `j0\nj `j1\n",
              src=[munchExp e1, munchExp e2], dst=[], jump=SOME [l1, l2]})
        | munchStm (T.CJUMP(T.NE, e1, e2, l1, l2)) =
              emit(ASM.OPER{assem="bne `s0, `s1, `j0\nj `j1\n",
              src=[munchExp e1, munchExp e2], dst=[], jump=SOME [l1, l2]})

			  | munchStm (T.LABEL lab) =
					emit(ASM.LABEL{assem = Symbol.name lab ^ ":\n",
								 lab = lab})

			  | munchStm (T.EXP e) = (munchExp e; ())

            (* Memory expressions *)
        and munchExp(T.MEM(T.CONST immed)) = result(fn dest =>
            emit(ASM.OPER{assem="lw `d0, " ^ removeSquiggle immed ^ "($0)\n",
                src=[],dst=[dest],jump=NONE})
            )
        |   munchExp(T.MEM(T.BINOP(T.PLUS, rs, T.CONST immed))) = result(fn dest =>
                emit(ASM.OPER{assem="lw `d0, " ^ removeSquiggle immed ^ "(`s0)\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.MEM(T.BINOP(T.MINUS, rs, T.CONST immed))) = result(fn dest =>
                emit(ASM.OPER{assem="lw `d0, " ^ removeSquiggle (~immed) ^ "(`s0)\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST immed, rs))) = result(fn dest =>
                emit(ASM.OPER{assem="lw `d0, " ^ removeSquiggle immed ^ "(`s0)\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.MEM(T.BINOP(T.MINUS, T.CONST immed, rs))) = result(fn dest =>
                emit(ASM.OPER{assem="lw `d0, " ^ removeSquiggle (~immed) ^ "(`s0)\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
            (* Expression of two consts, just replace with a singe li *)
        |   munchExp(T.BINOP(T.PLUS, T.CONST x, T.CONST y)) = result(fn dest =>
                emit(ASM.OPER{assem="li `d0, " ^ removeSquiggle (x+y) ^ "\n",
                src=[], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.MINUS, T.CONST x, T.CONST y)) = result(fn dest =>
                emit(ASM.OPER{assem="li `d0, " ^ removeSquiggle (x-y) ^ "\n",
                src=[], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.MUL, T.CONST x, T.CONST y)) = result(fn dest =>
                emit(ASM.OPER{assem="li `d0, " ^ removeSquiggle (x*y) ^ "\n",
                src=[], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.DIV, T.CONST x, T.CONST y)) = result(fn dest =>
                emit(ASM.OPER{assem="li `d0, " ^ removeSquiggle (floor (real(x)/real(y))) ^ "\n",
                src=[], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.AND, T.CONST x, T.CONST y)) = result(fn dest =>
                emit(ASM.OPER{assem="li `d0, " ^ removeSquiggle (if (x <> 0 andalso y <> 0) then 1 else 0) ^ "\n",
                src=[], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.OR, T.CONST x, T.CONST y)) = result(fn dest =>
                emit(ASM.OPER{assem="li `d0, " ^ removeSquiggle (if (x <> 0 orelse y <> 0) then 1 else 0) ^ "\n",
                src=[], dst=[dest], jump=NONE})
            )
            (* One immmediate expressions *)
        |   munchExp(T.BINOP(T.PLUS, rs, T.CONST immed)) = result(fn dest =>
                emit(ASM.OPER{assem="addi `d0, `s0, " ^ removeSquiggle immed ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.MINUS, rs, T.CONST immed)) = result(fn dest =>
                emit(ASM.OPER{assem="addi `d0, `s0, " ^ removeSquiggle(~immed) ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.AND, rs, T.CONST immed)) = result(fn dest =>
                emit(ASM.OPER{assem="andi `d0, `s0, " ^ removeSquiggle immed ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.OR, rs, T.CONST immed)) = result(fn dest =>
                emit(ASM.OPER{assem="ori `d0, `s0, " ^ removeSquiggle immed ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.MUL, rs, T.CONST immed)) =
                if immed = 0 then result(fn dest => (* Hey I know this one! *)
                    emit(ASM.OPER{assem="li `d0, 0\n",
                    src=[], dst=[dest], jump=NONE})
                    )
                else (
                    if immed > 0 andalso isShiftMult immed then result(fn dest => (* Dont mess with negative immmeds, that feels like a turing cliff problem *)
                            emit(ASM.OPER{assem="sll `d0, `s0, " ^ removeSquiggle (floor(numBits(immed))) ^ "\n",
                            src=[munchExp rs], dst=[dest], jump=NONE})
                        )
                    else result(fn dest =>
                        emit(ASM.OPER{assem="mul `d0, `s0, `s1\n",
                        src=[munchExp rs, munchExp (T.CONST immed)], dst=[dest], jump=NONE})
                        )
                    )
        |   munchExp(T.BINOP(T.DIV, rs, T.CONST immed)) =
                if immed = 0 then raise DivBy0 "Divide by zero found" (* Why wait till runtime to catch this? *)
                else (
                    if immed > 0 andalso isShiftMult immed then result(fn dest =>
                            emit(ASM.OPER{assem="sra `d0, `s0, " ^ removeSquiggle (floor(numBits(immed))) ^ "\n",
                            src=[munchExp rs], dst=[dest], jump=NONE})
                        )
                    else result(fn dest =>
                        emit(ASM.OPER{assem="div `d0, `s0, `s1\n",
                        src=[munchExp rs, munchExp (T.CONST immed)], dst=[dest], jump=NONE})
                        )
                    )
        |   munchExp(T.BINOP(T.PLUS, T.CONST immed, rs)) = result(fn dest =>
                emit(ASM.OPER{assem="addi `d0, `s0, " ^ removeSquiggle immed ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.MINUS, T.CONST immed, rs)) = result(fn dest =>
                emit(ASM.OPER{assem="addi `d0, `s0, " ^ removeSquiggle(~immed) ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.AND, T.CONST immed, rs)) = result(fn dest =>
                emit(ASM.OPER{assem="andi `d0, `s0, " ^ removeSquiggle immed ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.OR, T.CONST immed, rs)) = result(fn dest =>
                emit(ASM.OPER{assem="ori `d0, `s0, " ^ removeSquiggle immed ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.MUL, T.CONST immed, rs)) =
                if immed = 0 then result(fn dest =>
                    emit(ASM.OPER{assem="li `d0, 0\n",
                    src=[], dst=[dest], jump=NONE})
                    )
                else (
                    if immed > 0 andalso isShiftMult immed then result(fn dest =>
                            emit(ASM.OPER{assem="sll `d0, `s0, " ^ removeSquiggle (floor(numBits(immed))) ^ "\n",
                            src=[munchExp rs], dst=[dest], jump=NONE})
                        )
                    else result(fn dest =>
                        emit(ASM.OPER{assem="mul `d0, `s0, `s1\n",
                        src=[munchExp rs, munchExp (T.CONST immed)], dst=[dest], jump=NONE})
                        )
                    )
        |   munchExp(T.BINOP(T.DIV, T.CONST immed, rs)) =
                if immed = 0 then raise DivBy0 "Divide by zero found"
                else (
                    if immed > 0 andalso isShiftMult immed then result(fn dest =>
                            emit(ASM.OPER{assem="sra `d0, `s0, " ^ removeSquiggle (floor(numBits(immed))) ^ "\n",
                            src=[munchExp rs], dst=[dest], jump=NONE})
                        )
                    else result(fn dest =>
                        emit(ASM.OPER{assem="div `d0, `s0, `s1\n",
                        src=[munchExp rs, munchExp (T.CONST immed)], dst=[dest], jump=NONE})
                        )
                    )
        (* Two register expressions *)
        |   munchExp(T.BINOP(T.PLUS, rs, rt)) = result(fn dest =>
                emit(ASM.OPER{assem="add `d0, `s0, `s1\n",
                src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.MINUS, rs, rt)) = result(fn dest =>
                emit(ASM.OPER{assem="sub `d0, `s0, `s1\n",
                src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.MUL, rs, rt)) = result(fn dest =>
                emit(ASM.OPER{assem="mul `d0, `s0, `s1\n",
                src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.DIV, rs, rt)) = result(fn dest =>
                emit(ASM.OPER{assem="div `d0, `s0, `s1\n",
                src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.AND, rs, rt)) = result(fn dest =>
                emit(ASM.OPER{assem="and `d0, `s0, `s1\n",
                src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.OR, rs, rt)) = result(fn dest =>
                emit(ASM.OPER{assem="or `d0, `s0, `s1\n",
                src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
            )
      	|   munchExp(T.BINOP(T.XOR, rs, T.CONST immed)) = result(fn dest =>
                      emit(ASM.OPER{assem="xor `d0, `s0, " ^ removeSquiggle immed ^ "\n",
                      src=[munchExp rs], dst=[dest], jump=NONE})
            )
      	|   munchExp(T.BINOP(T.XOR, rs, rt)) = result(fn dest =>
                      emit(ASM.OPER{assem="xor `d0, `s0, `s1\n",
                      src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.LSHIFT, rs, T.CONST immed)) = result(fn dest =>
                emit(ASM.OPER{assem="sll `d0, `s0, " ^ removeSquiggle immed ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.LSHIFT, rs, rt)) = result(fn dest =>
                emit(ASM.OPER{assem="sllv `d0, `s0, `s1\n",
                src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.RSHIFT, rs, T.CONST immed)) = result(fn dest =>
                emit(ASM.OPER{assem="srl `d0, `s0, " ^ removeSquiggle immed ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.RSHIFT, rs, rt)) = result(fn dest =>
                emit(ASM.OPER{assem="srlv `d0, `s0, `s1\n",
                src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.ARSHIFT, rs, T.CONST immed)) = result(fn dest =>
                emit(ASM.OPER{assem="sra `d0, `s0, " ^ removeSquiggle immed ^ "\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.BINOP(T.ARSHIFT, rs, rt)) = result(fn dest =>
                emit(ASM.OPER{assem="srav `d0, `s0, `s1\n",
                src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
            )
        |   munchExp(T.MEM(rs)) = result(fn dest =>
                emit(ASM.OPER{assem="lw `d0, 0(`s0)\n",
                src=[munchExp rs], dst=[dest], jump=NONE})
            )
        |   munchExp(T.TEMP reg) = reg
        |   munchExp(T.CONST immed) = result(fn dest =>
                emit(ASM.OPER{assem="li `d0, " ^ removeSquiggle immed ^ "\n",
                src=[], dst=[dest], jump=NONE})
            )
        |   munchExp(T.NAME label) = result(fn dest =>
                emit(ASM.OPER{assem="la `d0, " ^ Symbol.name label ^ "\n",
                src=[], dst=[dest], jump=NONE})
            )
        |   munchExp(T.CALL(T.NAME funLabel, args)) =
                let 
                    val localsSize = (length args - length Frame.argregs)
                    val spOffset =  (2 + Int.max(0, localsSize)) * Frame.wordSize  
                    val raSaveLoc = T.MEM(T.BINOP(T.PLUS, T.TEMP Frame.SP, T.CONST spOffset))
                    val fpSaveLoc = T.MEM(T.BINOP(T.PLUS, T.TEMP Frame.SP, T.CONST (spOffset - Frame.wordSize)))

                    fun moveSPforRA (x) = T.MOVE(T.TEMP Frame.SP, T.BINOP(x, T.TEMP Frame.SP, T.CONST spOffset))

                in (
                    munchStm(moveSPforRA(T.MINUS));
                    munchStm(T.MOVE(raSaveLoc, T.TEMP Frame.ra)); (* Need to explicity save this *)
                    munchStm(T.MOVE(fpSaveLoc, T.TEMP Frame.FP));
                    result(fn dest =>
                            emit(ASM.OPER{assem="jal " ^ Symbol.name funLabel ^ "\n",
                            dst = Frame.ra :: Frame.callerSaves @ Frame.returnRegs,
                            src=(munchArgs(0, args, localsSize)), jump=NONE})
                        );
                    munchStm(T.MOVE(T.TEMP Frame.FP, fpSaveLoc));
                    munchStm(T.MOVE(T.TEMP Frame.ra, raSaveLoc));
                    munchStm(moveSPforRA(T.PLUS));
                    hd Frame.returnRegs)
                end

        and munchArgs(i, [], offset) = []
        |   munchArgs(i, exp::l, offset) =
                if i < length Frame.argregs then
                    let
                        val argReg = List.nth (Frame.argregs, i)
                    in
                        munchStm(T.MOVE(T.TEMP argReg, exp)); (* Putting exp here prevents unnecessary moves *)
                        argReg :: munchArgs(i+1,l,offset)
                    end
                else
                    let
                        val k = offset - (i - length Frame.argregs)
                        val byteOffset = k * Frame.wordSize
                        val oldReg = munchExp exp
                    in
                        munchStm(T.MOVE(
                            T.MEM(
                                T.BINOP(
                                    T.PLUS, T.TEMP Frame.SP, T.CONST byteOffset
                                    )
                                ), T.TEMP oldReg)); (*have to move anyway (sw), no point in passing exp directly*)
                        munchArgs(i+1, l,offset)
                    end
        in (
            munchStm stm;
            rev (!ilist))
        end
end
