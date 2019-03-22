structure Translate =
struct

    structure Frame : FRAME = MipsFrame
    structure A = Absyn
    datatype level = Top
                   | Lev of {parent: level, frame: Frame.frame} * Types.unique
    val root = Top
    type frag = Frame.frag
    val frags = ref([] : frag list)
    type access = level * Frame.access

    exception SyntaxException of string

    datatype exp = Ex of Tree.exp
                 | Nx of Tree.stm
                 | Cx of Temp.label * Temp.label -> Tree.stm

    fun handleInt(i: int) = Ex(Tree.CONST i)

    fun handleStr(s: string) =
         let
             val label = Temp.newlabel()
         in
             frags := Frame.STRING (label, s) :: !frags;
             Ex (Tree.NAME label)
         end

    fun handleNil() = Ex(Tree.CONST 0)

    fun newLevel({parent, name, formals}) = Lev(
        {parent=parent, frame=Frame.newFrame(
            {name=name, formals=true::formals}
        )}, ref()
    )

    fun getFormals(level) = (case level of
        Top => []
    |   Lev({parent=p, frame=f}, uniq') =>
            let
                val sl::formals = Frame.formals f
                fun f formal = (level, formal)
            in
                map f formals
            end
    )

    fun allocLocal(Top) = raise Match (*Y? let var x := 1 in x end*)
    |   allocLocal(l as Lev({frame, parent}, uniq')) = (fn(x) => (l, Frame.allocLocal(frame)(x)))

    fun seq([])   = Tree.EXP (Tree.CONST 0)
    |   seq([s])  = s
    |   seq(s::l) = Tree.SEQ(s, seq(l))

    fun unEx (Ex e) = e
    |   unEx (Cx c) =
            let
                val ret = Temp.newtemp()
                val true' = Temp.newlabel()
                val false' = Temp.newlabel()
            in
                Tree.ESEQ(seq([
                        Tree.MOVE(Tree.TEMP ret, Tree.CONST 1),
                        c(true', false'),
                        Tree.LABEL false',
                        Tree.MOVE(Tree.TEMP ret, Tree.CONST 0),
                        Tree.LABEL true'
                    ]), Tree.TEMP ret)
            end
    |   unEx (Nx n) = Tree.ESEQ(n, Tree.CONST 0)


    fun unNx (Ex e) = Tree.EXP(e)
    |   unNx (Cx c) =
            let
                val label = Temp.newlabel()
            in
                 Tree.SEQ(c(label, label), Tree.LABEL label)
            end
    |   unNx (Nx n) = n


    fun unCx (Ex (Tree.CONST 0)) = (fn(true', false') => Tree.JUMP(Tree.NAME false', [false']))
    |   unCx (Ex (Tree.CONST 1)) = (fn(true', false') => Tree.JUMP(Tree.NAME true',  [true']))
    |   unCx (Ex e) = (fn(true', false') => Tree.CJUMP(Tree.EQ, e, Tree.CONST 0, false', true'))
    |   unCx (Cx c) = c
    |   unCx (Nx n) = raise ErrorMsg.Error

    fun assign (left, right) = Nx (Tree.MOVE (unEx left, unEx right))

    fun whileExp(test, body, escape) =
        let
            val bodyLabel = Temp.newlabel()
            val testLabel = Temp.newlabel()
            val body' = unNx body
            val test' = unCx test
        in
            Nx(seq[
                Tree.LABEL testLabel,
                test'(bodyLabel, escape),
                Tree.LABEL bodyLabel,
                body',
                Tree.JUMP(Tree.NAME testLabel, [testLabel]),
                Tree.LABEL escape
                ])
        end

    fun forExp(iterVar, escape, low, high, b) =
        let
            val bodyLabel = Temp.newlabel()
            val forLabel = Temp.newlabel()
            val lo = unEx low
            val hi = unEx high
            val i = unEx iterVar
            val body = unNx b
        in
            Nx(seq[
                Tree.MOVE(i, lo),
                Tree.CJUMP(Tree.LE, i, hi, bodyLabel, escape),
                Tree.LABEL bodyLabel,
                body,
                Tree.CJUMP (Tree.LT, i, hi, forLabel, escape),
                Tree.LABEL forLabel,
                Tree.MOVE(i, Tree.BINOP (Tree.PLUS, i, Tree.CONST 1)),
                Tree.JUMP(Tree.NAME forLabel, [forLabel]),
                Tree.LABEL escape
                ]
            )
        end

    fun callExp (level: level, label, exps:exp list) = Ex(Tree.CALL(Tree.NAME(label), map unEx exps))

    fun letExp([], body)   = body
    |   letExp(decs, body) = Ex(Tree.ESEQ(seq(map unNx decs), unEx body))

    fun breakExp break = Nx (Tree.JUMP(Tree.NAME break, [break]))

    fun packMath(op', left, right) = Ex(Tree.BINOP(op', unEx left, unEx right))

    fun packCompare(op', left, right, NONE)   = Cx(fn(true', false') => Tree.CJUMP(op', unEx left, unEx right, true', false'))
    |   packCompare(op', left, right, SOME s: string option) = Ex(Frame.externalCall(s, [unEx left, unEx right]))

    fun intBinOps(A.PlusOp,   left, right) = packMath(Tree.PLUS,  left, right)
    |   intBinOps(A.MinusOp,  left, right) = packMath(Tree.MINUS, left, right)
    |   intBinOps(A.TimesOp,  left, right) = packMath(Tree.MUL,   left, right)
    |   intBinOps(A.DivideOp, left, right) = packMath(Tree.DIV,   left, right)
    |   intBinOps(A.NeqOp,    left, right) = packCompare(Tree.NE, left, right, NONE)
    |   intBinOps(A.EqOp,     left, right) = packCompare(Tree.EQ, left, right, NONE)
    |   intBinOps(A.GeOp,     left, right) = packCompare(Tree.GE, left, right, NONE)
    |   intBinOps(A.GtOp,     left, right) = packCompare(Tree.GT, left, right, NONE)
    |   intBinOps(A.LeOp,     left, right) = packCompare(Tree.LE, left, right, NONE)
    |   intBinOps(A.LtOp,     left, right) = packCompare(Tree.LT, left, right, NONE)

    fun strBinOps(A.NeqOp,    left, right) = packCompare(Tree.NE, left, right, SOME("strNE"))
    |   strBinOps(A.EqOp,     left, right) = packCompare(Tree.EQ, left, right, SOME("strE"))
    |   strBinOps(A.GeOp,     left, right) = packCompare(Tree.GE, left, right, SOME("strGTE"))
    |   strBinOps(A.GtOp,     left, right) = packCompare(Tree.GT, left, right, SOME("strGT"))
    |   strBinOps(A.LeOp,     left, right) = packCompare(Tree.LE, left, right, SOME("strLTE"))
    |   strBinOps(A.LtOp,     left, right) = packCompare(Tree.LT, left, right, SOME("strLT"))
    |   strBinOps(_,_,_)                   = raise SyntaxException "Unsupported string operation"

    fun calcMemOffset(base, offset) = Tree.MEM(Tree.BINOP(Tree.PLUS, base, offset))

    fun subscriptVar(base, offset) = Ex(calcMemOffset(unEx(base), Tree.BINOP(Tree.MUL, unEx(offset), Tree.CONST Frame.wordSize)))

    fun simpleVar(access, level) =
        let
            val (Lev(details, defref), defaccess) = access
            fun traceLink (level, access) =
                let
                    val Lev({parent, frame}, uniq') = level
                in
                    if uniq' = defref then
                        Frame.find(defaccess)(access)
                    else
                        let
                            val link::formals = Frame.formals frame
                        in
                            traceLink(parent, Frame.find(link)(access))
                        end
                end
        in
            Ex(traceLink(level, Tree.TEMP(Frame.FP)))
        end

    fun fieldVar(base, id, fields) =
        let
            fun indexOf(i, elem, []) = ~1
            |   indexOf(i, elem, a::l) = if elem = a then i else indexOf(i+1, elem, l)
            val index = indexOf(0, id, fields)
        in
            if index > ~1 then Ex(calcMemOffset(unEx(base), Tree.BINOP(Tree.MUL, Tree.CONST index, Tree.CONST Frame.wordSize)))
                          else raise SyntaxException "No such field for record"
        end

    fun ifThen(test, trueExp) =
        let
            val tru = Temp.newlabel()
            val fin = Temp.newlabel()
            val ret = Temp.newtemp()
            val cond = unCx(test)
        in case trueExp of
            Ex(e) => Ex (Tree.ESEQ (seq [
                                    (cond) (tru, fin),
                                    Tree.LABEL tru,
                                    Tree.MOVE (Tree.TEMP ret, e),
                                    Tree.LABEL fin
                                    ],
                                Tree.TEMP ret))
        |   Cx(c) => Cx (fn (t, f) => seq [
                                    (cond) (tru, fin),
                                    Tree.LABEL tru,
                                    (unCx trueExp) (t, f),
                                    Tree.LABEL fin
                                    ])
        |   Nx(n) => Nx (seq [
                            (cond) (tru, fin),
                            Tree.LABEL tru,
                            unNx trueExp,
                            Tree.LABEL fin
                            ])
        end

    fun ifThenElse (test, trueExp, falseExp) =
        let
            val ret = Temp.newtemp()
            val tru = Temp.newlabel()
            val fal = Temp.newlabel()
            val fin = Temp.newlabel()
            val cond = unCx(test)
        in case (trueExp, falseExp) of
            (Ex _, Ex _) => Ex (Tree.ESEQ (seq [(cond) (tru, fal),
                                        Tree.LABEL tru,
                                        Tree.MOVE (Tree.TEMP ret, unEx trueExp),
                                        Tree.JUMP (Tree.NAME fin, [fin]),
                                        Tree.LABEL fal,
                                        Tree.MOVE (Tree.TEMP ret, unEx falseExp),
                                        Tree.LABEL fin],
                                    Tree.TEMP ret))
        |   (Cx _, Cx _) => Cx (fn (t, f) =>
                                seq [(cond) (tru, fal),
                                Tree.LABEL tru,
                                (unCx trueExp) (t, f),
                                Tree.LABEL fal,
                                (unCx falseExp) (t, f)])
        |   (Nx _, Nx _) => Nx (seq [(cond) (tru, fal),
                                Tree.LABEL tru,
                                unNx trueExp,
                                Tree.JUMP (Tree.NAME fin, [fin]),
                                Tree.LABEL fal,
                                unNx falseExp,
                                Tree.LABEL fin])
        |   (_,_) => raise SyntaxException "Disagreement of if/else types"
        end

    fun ifWrapper(test, trueExp, falseExp) =
        if (isSome falseExp) then ifThenElse(test, trueExp, (valOf falseExp))
        else ifThen(test, trueExp)

    fun recordExp(fields) =
        let
            val ret = Temp.newtemp()
            val recSize = (length fields) * Frame.wordSize
            val allocateRecord =
                Tree.MOVE(
                    Tree.TEMP ret,
                    Frame.externalCall(
                        "recAlloc", [Tree.CONST(recSize)]
                    )
                )
            fun assignFields([], dex) = []
            |   assignFields(exp'::tail, dex) =
                    Tree.MOVE(
                        calcMemOffset(
                            Tree.TEMP ret,
                            Tree.CONST(dex * Frame.wordSize)
                        ),
                        unEx(exp')
                    ) :: assignFields(tail, dex+1)
        in
            Ex(Tree.ESEQ(
                seq(
                    allocateRecord :: assignFields(fields, 0)
                ),
                Tree.TEMP ret
            ))
        end

    fun seqExp [] = handleNil()
    |   seqExp [e] = e
    |   seqExp exps =
            let
                val len = length exps
                val tail = List.last exps
                val rest = List.take( exps, len-1)
            in
                Ex(Tree.ESEQ(seq(map unNx rest), unEx tail))
            end

    fun arrayExp(size, init) = Ex(Frame.externalCall("arrAlloc", [unEx size, unEx init]))

    fun diffLevel (Top) = 0
    |   diffLevel (l as Lev({parent: level,frame: Frame.frame},u: Types.unique)) = 1 + diffLevel(parent)

    fun traceSL (0, (lev: level)) = Tree.TEMP Frame.FP
    |   traceSL (delta, (l as Lev({parent, frame}, u))) = Frame.find(hd (Frame.formals frame)) (traceSL(delta-1, parent))

    (*Last arg is if the function has a result. If true, its a function,
    if false, it's a procedure. *)
    fun callExp(Lev({parent=Top,...},_), _, label, exps, true)  = Ex(Frame.externalCall(Symbol.name label, map unEx exps))
    |   callExp(Lev({parent=Top,...},_), _,label, exps, false) = Nx(Tree.EXP(Frame.externalCall(Symbol.name label, map unEx exps)))
    |   callExp(funLev, currLev, label, exps, true) =
            Ex(
                Tree.CALL(
                    Tree.NAME label,
                    traceSL(diffLevel currLev - diffLevel funLev, funLev)
                        :: (map unEx exps)
                )
            )
    |   callExp(funLev, currLev, label, exps, false) =
            Nx(
                Tree.EXP(
                    Tree.CALL(
                        Tree.NAME label,
                        traceSL(diffLevel currLev - diffLevel funLev, funLev)
                            :: (map unEx exps)
                    )
                )
            )
	fun getLabeled label = Ex (Tree.NAME(label))

end