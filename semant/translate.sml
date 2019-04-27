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
	type label = Temp.label
    exception SyntaxException of string

    datatype exp = Ex of Tree.exp
                 | Nx of Tree.stm
                 | Cx of Temp.label * Temp.label -> Tree.stm

	val namedlabel=Temp.namedlabel

    fun handleInt(i: int) = Ex(Tree.CONST i)

    fun searchForDup str [] = NONE
    |   searchForDup str ((Frame.PROC{...})::l) = searchForDup str l
    |   searchForDup str ((Frame.STRING(lab, s))::l) = case String.compare(str, s ) of
            EQUAL => SOME(Ex(Tree.NAME lab))
        |   _ => searchForDup str l

    fun handleStr(s: string) = case searchForDup s (!frags) of
            SOME(e) => e
        |   NONE =>
                let
                    val label = Temp.newlabel()
                in
                    frags := Frame.STRING (label, s) :: !frags;
                    Ex (Tree.NAME label)
                end

  	fun getLabel () = Temp.newlabel()

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
                val formals = tl (Frame.formals f)
                fun f' formal = (level, formal)
            in
                map f' formals
            end
    )

    fun allocLocal(l as Lev({parent, frame}, uniq)) (esc) =
      let
        val a = Frame.allocLocal(frame)(esc)
      in
        (l, a)
      end

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

	fun getAssign((Lev({parent = p,frame = f},u),a):access, exp) = case exp of Ex(Tree.ESEQ(e,r)) => Nx (Tree.MOVE ( (Frame.find a (Tree.TEMP Frame.FP)), Tree.MEM(unEx exp)))
												| _ => Nx (Tree.MOVE ((Frame.find a (Tree.TEMP Frame.FP)), unEx exp))

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

	  fun decsPre decs = foldr (fn (dec, lis) => case dec of Ex(Tree.CONST n) => lis
														| _ => dec::lis) [] decs

    fun letExp([], body)   = body
    |   letExp(decs, body) = Ex(Tree.ESEQ(seq(map unNx decs), unEx body))

    fun breakExp break = Nx (Tree.JUMP(Tree.NAME break, [break]))

    fun packMath(op', left, right) = Ex(Tree.BINOP(op', unEx left, unEx right))

    fun packCompare(op', left, right, NONE)   = Cx(fn(true', false') => Tree.CJUMP(op', unEx left, unEx right, true', false'))
    |   packCompare(op', left, right, SOME s: string option) = Ex(Frame.externalCall("tig_" ^ s, map unEx [left, right]))

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

    fun strBinOps(A.NeqOp,    left, right) = packCompare(Tree.NE, left, right, SOME("stringNotEqual"))
    |   strBinOps(A.EqOp,     left, right) = packCompare(Tree.EQ, left, right, SOME("stringEqual"))
    |   strBinOps(A.GeOp,     left, right) = packCompare(Tree.GE, left, right, SOME("stringGreaterThanEqual"))
    |   strBinOps(A.GtOp,     left, right) = packCompare(Tree.GT, left, right, SOME("stringGreaterThan"))
    |   strBinOps(A.LeOp,     left, right) = packCompare(Tree.LE, left, right, SOME("stringLessThanEqual"))
    |   strBinOps(A.LtOp,     left, right) = packCompare(Tree.LT, left, right, SOME("stringLessThan"))
    |   strBinOps(_,_,_)                   = raise SyntaxException "Unsupported string operation"

    fun calcMemOffset(base, offset) = Tree.MEM(Tree.BINOP(Tree.PLUS, base, offset))

    (*fun subscriptError offset pos = 
        let
            val lab = handleStr("Array out of bounds, quitting")
        in
            Ex(Frame.externalCall("tig_print", map unEx [handleNil(), lab]))
        end*)

    fun subscriptError offset pos = 
        let
            val t1 = Temp.newtemp()
            val t2 = Temp.newtemp()
            val t3 = Temp.newtemp()
            val t4 = Temp.newtemp()
            val asciiOffset = 48
        in
            Nx(seq[
                    Tree.MOVE(Tree.TEMP t1, Frame.externalCall("tig_chr", [Tree.BINOP(Tree.PLUS, unEx offset, Tree.CONST asciiOffset)])),
                    Tree.MOVE(Tree.TEMP t2, Frame.externalCall("tig_concat", [unEx (handleStr (ErrorMsg.getLine pos)), unEx(handleStr " Subscription Exception: Index ")])),
                    Tree.MOVE(Tree.TEMP t3, Frame.externalCall("tig_concat", [Tree.TEMP t2, Tree.TEMP t1])),
                    Tree.MOVE(Tree.TEMP t4, Frame.externalCall("tig_concat", [Tree.TEMP t3, unEx (handleStr " is out of bounds for the array")])),
                    Tree.EXP(Frame.externalCall("tig_print", [Tree.TEMP t4]))
                ])
        end

	fun subscriptVar(base, offset, pos) = let
										val true' = Temp.newlabel()
										val true'' = Temp.newlabel()
										val false' = Temp.newlabel()
									in
									Ex(Tree.ESEQ(seq([
									Tree.CJUMP(Tree.GT, calcMemOffset(unEx(base),Tree.CONST(~Frame.wordSize)), unEx offset, true', false'),
									Tree.LABEL true',
									Tree.CJUMP(Tree.GE,unEx offset,Tree.CONST 0 , true'', false'),
									Tree.LABEL false',
                                    unNx (subscriptError offset pos),
									Tree.EXP(Frame.externalCall("tig_exit", [])),
									Tree.LABEL true''
									]),
									calcMemOffset(unEx(base), Tree.BINOP(Tree.MUL, unEx(offset), Tree.CONST Frame.wordSize))
									))
									end

    fun traceSL (varLev as Lev({parent, frame}, u), stepLevel as Lev({parent=p, frame=f}, u') ) =
                if u=u' then Tree.TEMP Frame.FP else Tree.MEM(traceSL(varLev, p))
            |   traceSL (Top, _) =  ErrorMsg.impossible "Variable use at top level"
            |   traceSL (_, Top) =  ErrorMsg.impossible "Reached the top without finding access"

    fun simpleVar((varLev, acc), level) = case level of
        Top   => ErrorMsg.impossible "Var use at top level"
    |   Lev l => (case varLev of 
            Top => ErrorMsg.impossible "Var access at top level"
        |   Lev vl => Ex(Frame.find(acc)(traceSL(varLev, level)))
            )

    fun fieldVar(base, id, fields) =
        let
            fun indexOf(i, elem, []) = ~1
            |   indexOf(i, elem, a::l) = if elem = a then i else indexOf(i+1, elem, l)
            val index = indexOf(0, id, fields)
        in
            if index > ~1 then Ex(calcMemOffset(unEx(base), Tree.CONST (index * Frame.wordSize)))
                          else handleNil()
        end

    fun ifThen(test, trueExp) =
        let
            val t = Temp.newlabel()
            val j = Temp.newlabel()
            val ret = Temp.newtemp()
            val cond = unCx test
            val e1 = unEx trueExp
        in
            Nx(seq[ cond (t,j),
                    Tree.LABEL t,
                    Tree.EXP e1,
                    Tree.LABEL j]
                ) 
        end

    fun ifThenElse (test, trueExp, falseExp) =
        let
            val ret = Temp.newtemp()
            val t = Temp.newlabel()
            val f = Temp.newlabel()
            val j = Temp.newlabel()
            val cond = unCx(test)
            val e1 = unEx trueExp and e2 = unEx falseExp
        in 
            Ex(Tree.ESEQ(seq[ cond (t,f),
                              Tree.LABEL t,
                              Tree.MOVE(Tree.TEMP ret, e1),
                              Tree.JUMP(Tree.NAME j, [j]),
                              Tree.LABEL f,
                              Tree.MOVE(Tree.TEMP ret, e2),
                              Tree.LABEL j],
                            Tree.TEMP ret
                )) 
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
                        "tig_allocRecord", [Tree.CONST(recSize)]
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

    fun arrayExp(size, init) = 
            Ex(Tree.BINOP(Tree.PLUS, Tree.CONST Frame.wordSize, Frame.externalCall("tig_initArray", map unEx [size, init])))

    fun getUniq (Lev({parent, frame}, unique)) = unique

    fun getParent (Lev({parent, frame}, unique)) = parent

    fun callExp(Top, currLev, label, exps) = 
            Ex(
                Tree.CALL(
                        Tree.NAME label,
                        map unEx exps
                    )
                )
    |   callExp(funLev as Lev({parent, frame}, uniq), currLev, label, exps) = 
        let
            
        in
            Ex(
                Tree.CALL(
                        Tree.NAME label, 
                        traceSL(parent, currLev) :: map unEx exps
                    )
                )
        end

	fun procEntryExit {level = Lev({parent=pa, frame=frame}, u), body=exp} =
        frags := !frags @ [Frame.PROC{
                body=Frame.procEntryExit1(
                        frame,
                        Tree.MOVE(
                            Tree.TEMP(
                                hd Frame.returnRegs
                            ),
                            unEx exp
                        )
                    ),
                frame=frame
            }]

	fun getResult () = rev (!frags)

	fun reset () = frags := []

end
