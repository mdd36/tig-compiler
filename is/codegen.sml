structure ASM = Assem
structure T = Tree

signature CODEGEN =
sig
    structure Frame : FRAME
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure Codegen :> CODEGEN =
struct
    exception ArgCount of string
    structure Frame = MipsFrame

    fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
        let
            val ilist = ref []
            fun emit x = ilist := x :: !ilist
            fun result (gen) = let val t = Temp.newtemp() in gen t; t end
            fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)

			  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)),e2)) =
					emit(ASM.OPER{assem = "sw 's1, " ^ Int.toString i ^ "('s0)\n",
								src = [munchExp e1, munchExp e2],
								dst = [],
								jump = NONE})

			  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)),e2)) =
					emit(ASM.OPER{assem = "sw 's1, " ^ Int.toString i ^ "('s0)\n",
								src = [munchExp e1, munchExp e2],
								dst = [],
								jump = NONE})

			  | munchStm (T.MOVE(T.MEM(e1), T.MEM(e2))) =
					emit(ASM.OPER{assem = "move 's0, 's1\n",
								src = [munchExp e1, munchExp e2],
								dst = [],
								jump = NONE})

			  | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) =
					emit(ASM.OPER{assem = "sw 's0, " ^ Int.toString i ^ "($0)\n",
								src = [munchExp e2],
								dst = [],
								jump = NONE})

			  | munchStm (T.MOVE(T.MEM(e1), e2)) =
					emit(ASM.OPER{assem = "sw 's1, 0('s0)\n",
								src = [munchExp e1, munchExp e2],
								dst = [],
								jump = NONE})

			  | munchStm (T.MOVE(T.TEMP i, e2)) =
					emit(ASM.OPER{assem = "sw 's1, 0('s0)\n",
								src = [munchExp e2],
								dst = [i],
								jump = NONE})

			  | munchStm (T.JUMP(e, labs)) =
					emit(ASM.OPER{assem = "j 's0\n",
								src = [munchExp e],
								dst = [],
								jump = SOME labs})

			  | munchStm (T.LABEL lab) =
					emit(ASM.LABEL{assem = Symbol.name lab ^ ":\n",
								 lab = lab})

			  | munchStm (T.EXP e) = (munchExp e; ())

            and munchExp(T.MEM(T.CONST immed)) = result(fn dest =>
                emit(ASM.OPER{assem="lw `d0, " ^ Int.toString immed ^ "($0)",
                    src=[],dst=[dest],jump=NONE})
                )
            |   munchExp(T.MEM(T.BINOP(T.PLUS, rs, T.CONST immed))) = result(fn dest =>
                    emit(ASM.OPER{assem="lw `d0, " ^ Int.toString immed ^ "(`s0)",
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.MEM(T.BINOP(T.MINUS, rs, T.CONST immed))) = result(fn dest =>
                    emit(ASM.OPER{assem="lw `d0, " ^ Int.toString (~immed) ^ "(`s0)",
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST immed, rs))) = result(fn dest =>
                    emit(ASM.OPER{assem="lw `d0, " ^ Int.toString immed ^ "(`s0)",
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.MEM(T.BINOP(T.MINUS, T.CONST immed, rs))) = result(fn dest =>
                    emit(ASM.OPER{assem="lw `d0, " ^ Int.toString (~immed) ^ "(`s0)",
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.PLUS, rs, T.CONST immed)) = result(fn dest =>
                    emit(ASM.OPER{assem="addi `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.MINUS, rs, T.CONST immed)) = result(fn dest =>
                    emit(ASM.OPER{assem="subi `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.AND, rs, T.CONST immed)) = result(fn dest =>
                    emit(ASM.OPER{assem="andi `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.OR, rs, T.CONST immed)) = result(fn dest =>
                    emit(ASM.OPER{assem="ori `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            (* |   munchExp(T.BINOP(T.MUL, rs, T.CONST immed)) = result(result(fn dest =>

                ) *)
            (* |   munchExp(T.BINOP(T.MUL, rs, T.CONST immed)) = result(fn dest =>
                    emit(ASM.OPER{assem="li "})
                ) TODO muli?*)
            |   munchExp(T.BINOP(T.PLUS, T.CONST immed, rs)) = result(fn dest =>
                    emit(ASM.OPER{assem="addi `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.MINUS, T.CONST immed, rs)) = result(fn dest =>
                    emit(ASM.OPER{assem="subi `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.AND, T.CONST immed, rs)) = result(fn dest =>
                    emit(ASM.OPER{assem="andi `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.OR, T.CONST immed, rs)) = result(fn dest =>
                    emit(ASM.OPER{assem="ori `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )

            |   munchExp(T.BINOP(T.PLUS, rs, rt)) = result(fn dest =>
                    emit(ASM.OPER{assem="add `d0, `s0, `s1",
                    src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.MINUS, rs, rt)) = result(fn dest =>
                    emit(ASM.OPER{assem="sub `d0, `s0, `s1",
                    src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.MUL, rs, rt)) = result(fn dest =>
                    emit(ASM.OPER{assem="mul `d0, `s0, `s1",
                    src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.DIV, rs, rt)) = result(fn dest =>
                    emit(ASM.OPER{assem="div `d0, `s0, `s1",
                    src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.AND, rs, rt)) = result(fn dest =>
                    emit(ASM.OPER{assem="and `d0, `s0, `s1",
                    src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.OR, rs, rt)) = result(fn dest =>
                    emit(ASM.OPER{assem="or `d0, `s0, `s1",
                    src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.LSHIFT, rs, T.CONST immed)) = result(fn dest =>
                    emit(ASM.OPER{assem="sll `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.LSHIFT, rs, rt)) = result(fn dest =>
                    emit(ASM.OPER{assem="sllv `d0, `s0, `s1",
                    src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.RSHIFT, rs, T.CONST immed)) = result(fn dest =>
                    emit(ASM.OPER{assem="srl `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.RSHIFT, rs, rt)) = result(fn dest =>
                    emit(ASM.OPER{assem="srlv `d0, `s0, `s1",
                    src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.ARSHIFT, rs, T.CONST immed)) = result(fn dest =>
                    emit(ASM.OPER{assem="sra `d0, `s0, " ^ Int.toString immed,
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.BINOP(T.ARSHIFT, rs, rt)) = result(fn dest =>
                    emit(ASM.OPER{assem="srav `d0, `s0, `s1",
                    src=[munchExp rs, munchExp rt], dst=[dest], jump=NONE})
                )
            |   munchExp(T.MEM(rs)) = result(fn dest =>
                    emit(ASM.OPER{assem="lw `d0, 0(`s0)",
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.TEMP reg) = reg
            |   munchExp(T.CONST immed) = result(fn dest =>
                    emit(ASM.OPER{assem="li `d0, " ^ Int.toString immed,
                    src=[], dst=[dest], jump=NONE})
                )
            |   munchExp(T.NAME label) = result(fn dest =>
                    emit(ASM.OPER{assem="la `d0, " ^ Symbol.name label,
                    src=[], dst=[dest], jump=NONE})
                )
            |   munchExp(T.CALL(rs, args)) =
                    let
                        val callerSavePairs = map (fn reg => (Temp.newtemp(), reg)) Frame.callerSaves
                        val srcs = map #1 callerSavePairs

                        fun save r1 r2 = T.MOVE(T.TEMP r2, T.TEMP r1)
                        fun restore r1 r2 = T.MOVE(T.TEMP r1, T.TEMP r2)
                    in (
                        map (fn (a,r) => munchStm(save a r)) callerSavePairs;
                        result(fn dest =>
                                emit(ASM.OPER{assem="jal `s0", dst=Frame.callerSaves,
                                src=(munchExp(rs) :: munchArgs(0, args)), jump=NONE})
                            );
                        map (fn (a,r) => munchStm(restore a r)) (rev callerSavePairs);
                        Frame.returnReg)
                    end

            and munchArgs(i, []) = []
            |   munchArgs(i, exp::l) = (
                    if i < 4 then
                        let
                            val argReg = List.nth (Frame.argregs, i)
                            val oldReg = munchExp(exp)
                        in
                            (munchStm(T.MOVE(T.TEMP argReg, T.TEMP oldReg));
                            argReg :: munchArgs(i+1,l))
                        end
                    else raise ArgCount "More than 4 arguments provided") (* FIXME This compiler would be garbage if this isn't added *)
        in (
            munchStm stm;
            rev (!ilist))
        end
end
