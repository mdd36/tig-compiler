structure MipsFrame : FRAME =
struct
    datatype access = InReg of Temp.temp | InFrame of int
    type frame = {name: Temp.label, formals: bool list, locals: int ref}

    val wordSize = 4

    fun newFrame {name, formals} = {name=name, formals=formals, locals=ref 0}
    fun formals {name, formals, locals} =
        let
            fun findAccess (formal, offset) = if formal then InFrame(offset * wordSize) (*Going up the stack by machine word increments*)
                                           else InReg(Temp.newtemp())
            fun buildOffsets([], depth) = []
            |   buildOffsets(a::l, depth) = if a then depth::buildOffsets(l, depth+1) else depth::buildOffsets(l, depth)
            val offsets = buildOffsets(formals, 0)
        in
            ListPair.foldr (fn(formal, offset, res) => findAccess(formal, offset)::res) [] (formals, offsets)
        end
    fun allocLocal {name, formals, locals} =
        fn bool' => (
            let
                fun findAccess escapes offset = if escapes then (!offset = !offset + 1; InFrame((!offset)*(~wordSize)))
                                                else InReg(Temp.newtemp())
            in
                findAccess bool' locals
            end
        )

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

    val zero = Temp.newtemp()

    val v0 = Temp.newtemp()

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

    val SP = Temp.newtemp()
    val FP = Temp.newtemp()
    val ra = Temp.newtemp()

    val argregs = [a0, a1, a2, a3]
    val temps = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9]
    val calleeSaves = [s0, s1, s2, s3, s4, s5, s6, s7]
    val callerSaves = [ra, FP, SP] @ temps
    val returnReg = v0

    fun find(InFrame(depth))  = (fn (fp) => Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST(depth))))
    |   find(InReg(reg))      = (fn (fp) => Tree.TEMP(reg))

    fun externalCall(name, args) = Tree.CALL(Tree.NAME(Temp.namedlabel name), args)


end
