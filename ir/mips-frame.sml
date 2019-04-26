structure MipsFrame : FRAME =
struct

    datatype access = InReg of Temp.temp | InFrame of int
    type frame = {name: Temp.label, formals: access list, locals: int ref}
    type register = string
    val wordSize = 4
    val K = 10 (* 9 temp reg's in MIPS *)

    fun newFrame {name, formals} = 
        let
            fun allocFormal(escapes::l, offset) = 
                    if escapes then InFrame(offset)::allocFormal(l, offset+wordSize)
                    else InReg(Temp.newtemp())::allocFormal(l, offset)
            |   allocFormal([],offset) = []

            val f = allocFormal(formals, 0)

            fun countFrameFormal ([], x) = x
            |   countFrameFormal (InFrame(y)::l, x) = countFrameFormal(l, x+1)
            |   countFrameFormal (InReg(y)::l, x) = countFrameFormal(l, x)
        in
            {name=name, formals=f, locals=ref (countFrameFormal(f, 0))}
        end
    
    fun formals {name, formals=f, locals} = f

    fun allocLocal {name, formals, locals} =
        fn bool' => (
            let
                fun findAccess escapes offset = if escapes then let val off = !offset; val _ = offset := !offset + 1 in InFrame((off)*(~wordSize)) end
                                                else InReg(Temp.newtemp())
            in
                findAccess bool' locals
            end
        )

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

    

    fun string (STRING(lab,s)) = Symbol.name lab ^":\n.word " ^ Int.toString(String.size(s)) ^ "\n.asciiz \"" ^ s ^ "\"\n"

    val zero = Temp.newtemp()

    val at = Temp.newtemp()

    val v0 = Temp.newtemp()
    val v1 = Temp.newtemp()

    val a0 = Temp.newtemp()
    val a1 = Temp.newtemp()
    val a2 = Temp.newtemp()
    val a3 = Temp.newtemp()

    val t0 = Temp.newtemp()
    val t1 = Temp.newtemp()
    val t2 = Temp.newtemp()
    val t3 = Temp.newtemp()
    val t4 = Temp.newtemp()
    val t5 = Temp.newtemp()
    val t6 = Temp.newtemp()
    val t7 = Temp.newtemp()

    val s0 = Temp.newtemp()
    val s1 = Temp.newtemp()
    val s2 = Temp.newtemp()
    val s3 = Temp.newtemp()
    val s4 = Temp.newtemp()
    val s5 = Temp.newtemp()
    val s6 = Temp.newtemp()
    val s7 = Temp.newtemp()

    val t8 = Temp.newtemp()
    val t9 = Temp.newtemp()

    val k0 = Temp.newtemp()
    val k1 = Temp.newtemp()

    val SP = Temp.newtemp()
    val FP = Temp.newtemp()
    val ra = Temp.newtemp()

    val argregs = [a0, a1, a2, a3]
    val temps = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9]
    val calleeSaves = [s0, s1, s2, s3, s4, s5, s6, s7]
    val callerSaves = [ra, FP, SP] @ temps
    val returnRegs = [v0, v1]
    val sysReseverd = [at, k0, k1]

    val tempMap = Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                    Temp.enter(
                        Temp.empty, zero, "zero" :register
                        ), v0, "v0" :register
                    ), v1, "v1" :register
                    ), a0, "a0" :register
                    ), a1, "a1" :register
                    ), a2, "a2" :register
                    ), a3, "a3" :register
                    ), t0, "t0" :register
                    ), t1, "t1" :register
                    ), t2, "t2" :register
                    ), t3, "t3" :register
                    ), t4, "t4" :register
                    ), t5, "t5" :register
                    ), t6, "t6" :register
                    ), t7, "t7" :register
                    ), t8, "t8" :register
                    ), t9, "t9" :register
                    ), s0, "s0" :register
                    ), s1, "s1" :register
                    ), s2, "s2" :register
                    ), s3, "s3" :register
                    ), s4, "s4" :register
                    ), s5, "s5" :register
                    ), s6, "s6" :register
                    ), s7, "s7" :register
                    ), SP, "sp" :register
                    ), FP, "fp" :register
                    ), ra, "ra" :register
                    ), at, "at" :register
                    ), k0, "k0" :register
                    ), k1, "k1" :register
                    )

    fun makestring t = if isSome(Temp.look(tempMap,t)) then valOf(Temp.look(tempMap,t)) else Temp.makestring t
    
    fun makestring2 allocation t = valOf(Temp.look(allocation,t))

    fun registerColors() = map (fn x => valOf(Temp.Table.look(tempMap, x))) (temps @ calleeSaves)

    fun name {name, formals, locals} = Symbol.name name

    fun findDepth(InFrame(depth)) = if depth >= 0 then depth else ~depth
    |   findDepth(InReg(x)) = raise ErrorMsg.impossible "Don't call me"

    fun find(InFrame(depth))  = (fn (fp) => Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST(depth))))  (* TODO this is so hacky *)
    |   find(InReg(reg))      = (fn (fp) => Tree.TEMP(reg))

    fun externalCall(name, args) = Tree.CALL(Tree.NAME(Temp.namedlabel name), args)

    fun seq([])   = Tree.EXP (Tree.CONST 0) (* Just copying this here to avoid circular dependencies -- TODO literally anything but this *)
    |   seq([s])  = s
    |   seq(s::l) = Tree.SEQ(s, seq(l))

    fun munchArgs([], _, _, _) = []
    |   munchArgs(InReg(t)::l, argReg::a, locals, depth) =
            Tree.MOVE(Tree.TEMP t, Tree.TEMP argReg) :: munchArgs(l, a, locals, depth)
    |   munchArgs(InReg(t)::l, [], locals, depth) =
            let
                val offSet = (locals + depth) * wordSize (* + 1 here since we need to step over the SL *)
            in
                Tree.MOVE(
                    Tree.TEMP t,
                    Tree.MEM(
                        Tree.BINOP(
                            Tree.PLUS, Tree.TEMP FP, Tree.CONST offSet (* Read relative to FP *)
                            )
                        )
                    ) :: munchArgs(l, [], locals, depth+1)
            end
    |   munchArgs(InFrame(j)::l, argReg::a, locals, depth) =
            Tree.MOVE(
                Tree.MEM(
                    Tree.BINOP(
                        Tree.PLUS, Tree.TEMP FP, Tree.CONST j (* Store relative to SP *)
                        )
                    ),
                Tree.TEMP argReg
                ) :: munchArgs(l, a, locals, depth)
    |   munchArgs(InFrame(j)::l, [], locals, depth) =
            let
                val offSet = (locals + depth) * wordSize (* + 1 here since we need to step over the SL *)
            in
                Tree.MOVE(
                        Tree.MEM(
                            Tree.BINOP(
                                Tree.PLUS, Tree.TEMP FP, Tree.CONST j (* Store relative to SP *)
                                )
                            ),
                        Tree.MEM(
                            Tree.BINOP(
                                Tree.PLUS, Tree.TEMP FP, Tree.CONST offSet (* Read relative to FP *)
                                )
                            )
                    ) :: munchArgs(l, [], locals, depth+1)
            end

    fun procEntryExit1(frame as {name=name, formals=f, locals=locals}: frame, body) =
          let
           (* val setNewFP = Tree.MOVE(Tree.TEMP FP, Tree.TEMP SP)*)(*Tree.BINOP(Tree.MINUS, Tree.TEMP SP, Tree.CONST (wordSize)))*)
          in
            seq(Tree.LABEL name :: munchArgs(f, argregs, !locals, 0) @ [body])
          end

    fun removeSquiggle x = if x < 0 then "-" ^ Int.toString (~x) else Int.toString x


    fun procEntryExit2(frame, body) =
		(body @ [Assem.OPER{assem="", src=(zero :: calleeSaves @ sysReseverd), dst=[], jump=SOME[]}])


    fun correctOffset ((a as Assem.OPER{assem, src, dst, jump})::l) count = 
        let
            (* lw $f0 0($fp) *)
           (* val _ = print("OPER\n")*)
            fun ret offset = let 
                val assemStr = "lw `d0, " ^ removeSquiggle offset ^ "(`s0)\n"
            in Assem.OPER{assem=assemStr, src=src, dst=dst, jump=jump} end

            fun suff() = case String.compare(makestring (hd src), "fp") of 
                EQUAL => ((*print("SUFFIX\n");*) true)
            |   _     => ((*print("NOT SUFFIX: " ^ makestring (hd src) ^ "\n");*)false)
        in
            if String.isPrefix "lw" assem andalso suff() then ret(count) :: (correctOffset l (count + wordSize))
            else a :: (correctOffset l count)
        end
    |   correctOffset (x::l) count = let val _ = print("OTHER\n") in  x :: (correctOffset l count) end
    |   correctOffset [] count = []

    fun procEntryExit3(frame as {name, formals, locals}, body) =
            let
                val hd' = hd body (*label*)
                val tl' = tl body (*function assembly*)
                handle Empty => []

                val stackArgs = Int.max(0, length formals - length argregs)

              (*  fun f (Assem.OPER{assem, ...}) = print(assem)
                |   f (Assem.MOVE{assem, ...}) = print(assem)
                |   f (_) = ()*)

                val bod = if stackArgs > 0 then 
                    let
                        val droppedRegArgs = List.drop(tl', length argregs)
                        val assemToRewrite = List.take(droppedRegArgs, stackArgs)
                        val rewrittenAssem = correctOffset assemToRewrite ((!locals+1) * wordSize)
                        val rest = List.drop(droppedRegArgs, stackArgs)
                       (* val _ = print("\n")*)
                    in
                        List.take(tl', length argregs) @ rewrittenAssem @ rest
                    end
                else tl'

                val raAcc = allocLocal(frame)(true)
                val storeRA = Assem.OPER{assem="sw $ra, " ^ removeSquiggle (findDepth raAcc) ^ "($fp)\n", src=[ra], dst=[], jump=NONE} 
                val loadRA  = Assem.OPER{assem="lw $ra, " ^ removeSquiggle (findDepth raAcc) ^ "($fp)\n", src=[ra], dst=[], jump=NONE}

                val offSet = (!locals) * wordSize
                fun moveSP dir = Assem.OPER{assem=if offSet > 0 then "addi $sp, $sp, " ^ dir ^ Int.toString(offSet) ^ "\n" else "",
                                        src=[],dst=[],jump=NONE}

                val moveFP = Assem.MOVE{assem="move `d0, `s0\n", src=SP, dst=FP}

                val jr = Assem.OPER{assem="jr `d0\n\n", src=[], dst=[ra], jump=SOME[]}
            in
                {prolog= "# PROCEDURE " ^ Symbol.name(#name frame) ^ "\n",
                body= hd' :: moveSP("-") :: moveFP :: storeRA :: bod @ (loadRA :: moveSP("") :: jr :: []),
                epilog="# END " ^ Symbol.name(#name frame) ^ "\n"}

            end
    end
    
