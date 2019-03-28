structure A = Assem
structure T = Tree

signature CODEGEN =
sig
    structure Frame : Frame
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure Codegen :> CODEGEN =
struct
    exception ArgCount of string
    structure Frame = MipsFrame

    fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
        let
            val ilist = ref ([]: A.instr list)
            fun emit x = ilist : x :: !ilist
            fun result (gen) = let val t = Temp.newtemp() in gen t; t end
            fun munchStm(_) = ()
            and munchExp(T.MEM(T.CONST immed)) = result(fn dest =>
                emit(A.OPER{assem="lw `d0, " ^ Int.toString immed ^ "($0)",
                    src=[],dst=[dest],jump=NONE})
                )
            |   munchExp(T.MEM(T.BINOP(T.PLUS, rs, T.CONST immmed))) = result(fn dest =>
                    emit(A.OPER{assem="lw `d0, " ^ Int.toString immmed ^ "(`s0)",
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   muchExp(T.MEM(T.BINOP(T.MINUS, rs, T.CONST immed))) = result(fn dest =>
                    emit(A.OPER{assem="lw `d0, " ^ Int.toString (~immmed) ^ "(`s0)",
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST immmed, rs))) = result(fn dest =>
                    emit(A.OPER{assem="lw `d0, " ^ Int.toString immmed ^ "(`s0)",
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   muchExp(T.MEM(T.BINOP(T.MINUS, T.CONST immed, rs))) = result(fn dest =>
                    emit(A.OPER{assem="lw `d0, " ^ Int.toString (~immmed) ^ "(`s0)",
                    src=[munchExp rs], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.PLUS, rs, T.CONST immed)) = result(fn dest =>
                    emit(A.OPER{assem="addi `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.MINUS, rs, T.CONST immed)) = result(fn dest =>
                    emit(A.OPER{assem="subi `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE}})
                )
            |   muchExp(T.BINOP(T.AND, rs, T.CONST immed)) = result(fn dest =>
                    emit(A.OPER{assem="andi `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE}})
                )
            |   muchExp(T.BINOP(T.OR, rs, T.CONST immed)) = result(fn dest =>
                    emit(A.OPER{assem="ori `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE}})
                )
            (* |   muchExp(T.BINOP(T.MUL, rs, T.CONST immed)) = result(fn dest =>
                    emit(A.OPER{assem="li "})
                ) TODO muli?*)
            |   muchExp(T.BINOP(T.PLUS, T.CONST immed, rs)) = result(fn dest =>
                    emit(A.OPER{assem="addi `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE}})
                )
            |   muchExp(T.BINOP(T.MINUS, T.CONST immed, rs)) = result(fn dest =>
                    emit(A.OPER{assem="subi `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE}})
                )
            |   muchExp(T.BINOP(T.AND, T.CONST immed, rs)) = result(fn dest =>
                    emit(A.OPER{assem="andi `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE}})
                )
            |   muchExp(T.BINOP(T.OR, T.CONST immed, rs)) = result(fn dest =>
                    emit(A.OPER{assem="ori `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE}})
                )

            |   muchExp(T.BINOP(T.PLUS, rs, rt)) = result(fn dest =>
                    emit(A.OPER{assem="add `d0, `s0, `s1",
                    src=[muchExp rs, muchExp rt], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.SUB, rs, rt)) = result(fn dest =>
                    emit(A.OPER{assem="sub `d0, `s0, `s1",
                    src=[muchExp rs, muchExp rt], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.MUL, rs, rt)) = result(fn dest =>
                    emit(A.OPER{assem="mul `d0, `s0, `s1",
                    src=[muchExp rs, muchExp rt], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.DIV, rs, rt)) = result(fn dest =>
                    emit(A.OPER{assem="div `d0, `s0, `s1",
                    src=[muchExp rs, muchExp rt], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.AND, rs, rt)) = result(fn dest =>
                    emit(A.OPER{assem="and `d0, `s0, `s1",
                    src=[muchExp rs, muchExp rt], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.OR, rs, rt)) = result(fn dest =>
                    emit(A.OPER{assem="or `d0, `s0, `s1",
                    src=[muchExp rs, muchExp rt], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.LSHIFT(rs, T.CONST immed))) = result(fn dest =>
                    emit(A.OPER{assem="sll `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.LSHIFT(rs, rt))) = result(fn dest =>
                    emit(A.OPER{assem="sllv `d0, `s0, `s1",
                    src=[muchExp rs, muchExp rt], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.RSHIFT(rs, T.CONST immed))) = result(fn dest =>
                    emit(A.OPER{assem="srl `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.RSHIFT(rs, rt))) = result(fn dest =>
                    emit(A.OPER{assem="srlv `d0, `s0, `s1",
                    src=[muchExp rs, muchExp rt], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.ARRSHIFT(rs, T.CONST immed))) = (fn dest =>
                    emit(A.OPER{assem="sra `d0, `s0, " ^ Int.toString immed,
                    src=[muchExp rs], dst=[dest], jump=NONE})
                )
            |   muchExp(T.BINOP(T.ARRSHIFT(rs, rt))) = result(fn dest =>
                    emit(A.OPER{assem="srav `d0, `s0, `s1",
                    src=[muchExp rs, muchExp rt], dst=[dest], jump=NONE})
                )
            |   muchExp(T.MEM(rs)) = result(fn dest =>
                    emit(A.OPER{assem="lw `d0, 0(`s0)",
                    src=[muchExp rs], dst=[dest], jump=NONE})
                )
            |   munchExp(T.TEMP reg) = reg
            |   muchExp(T.CONST immed) = result(fn dest =>
                    emit(A.OPER{assem="li `d0, " ^ Int.toString immed,
                    src=[], dst=[dest], jump=NONE})
                )
            |   muchExp(T.NAME label) = result(fn dest =>
                    emit(A.OPER{assem="la `d0, " ^ Symbol.name label,
                    src=[], dst=[dest], jump=NONE})
                )
            |   muchExp(T.CALL(rs, args)) =
                    let
                        val callerSavePairs = map (fn reg => (Temp.newtemp(), reg)) Frame.callerSaves
                        srcs = map #1 callerSavePairs

                        fun save r1 r2 = T.MOVE(T.TEMP r2, T.TEMP r1)
                        fun restore r1 r2 = T.MOVE(T.TEMP r1, T.TEMP r2)
                    in
                        map (fn (a,r) => munchStm(save a r)) pairs;
                        result(fn dest =>
                                emit(A.OPER{assem="jal `s0", dest=callTrashed
                                src=[munchExp(rs) :: muchArgs(0, args)], jump=NONE})
                            );
                        map (fn (a,r) => munchStm(restore a r)) (rev pairs);
                        FRAME.V0
                    end

            and muchArgs(i, []) = []
            |   muchArgs(i, exp::l) =
                    if i < 4 then
                        let
                            val argReg = List.nth (Frame.argregs, i)
                            val oldReg = muchExp(exp)
                        in
                            munchStm(T.MOVE(T.TEMP argReg, T.TEMP oldReg));
                            dst :: munchArgs(i+1,l)
                        end
                    else raise ArgCount "More than 4 arguments provided" (* FIXME This compiler would be garbage if this isn't added *)
        in
            munchStm stm;
            rev (!ilist)
        end
end
