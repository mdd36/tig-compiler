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

    fun breakExp break = Nx (Tree.JUMP(Tree.NAME break, [break]))

    fun binop(op', left, right) = Ex(Tree.BINOP(op', unEx left, unEx right))

    fun relop(left, right, operator) =
        let
            val translatedOp = case operator of
                A.LeOp => Tree.LE
            |   A.LtOp => Tree.LT
            |   A.GeOp => Tree.GE
            |   A.GtOp => Tree.GT
            |   A.EqOp => Tree.EQ
            |   A.NeqOp => Tree.NE
        in
            Cx( fn(true', false') => Tree.CJUMP(translatedOp, unEx(left), unEx(right), true', false') )
        end

    fun callExp (level: level, label, exps:exp list) = Ex(Tree.CALL(Tree.NAME(label), map unEx exps))

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
            Ex(calcMemOffset(unEx(base), Tree.BINOP(Tree.MUL, Tree.CONST index, Tree.CONST Frame.wordSize)))
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
        end

end
