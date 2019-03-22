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

    val FP = Temp.newtemp()
    val SP = Temp.newtemp()

    fun find(InFrame(depth))  = (fn (fp) => Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST(depth))))
    |   find(InReg(reg))      = (fn (fp) => Tree.TEMP(reg))

    fun externalCall(name, args) = Tree.CALL(Tree.NAME(Temp.namedlabel name), args)
end
