structure A = Absyn
structure set =  RedBlackSetFn(type ord_key=Symbol.symbol val compare=Symbol.compare)
structure mymap =  RedBlackMapFn(type ord_key=Symbol.symbol val compare=Symbol.compare)
structure TR = Translate

structure Semant :
  sig
    type expty = {exp: TR.exp, ty: Types.ty}
    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    val transProg: A.exp -> TR.frag list;
    val transVar: venv * tenv * A.var * TR.level * TR.label -> expty
    val transExp: venv * tenv * A.exp * TR.level * TR.label -> expty
    val transDec: venv * tenv * A.dec * TR.level * TR.label -> {venv: venv, tenv: tenv, exp: TR.exp}
    val transTy :        tenv * A.ty  -> Types.ty
	val venv:venv
	val tenv:tenv
  end =
struct

  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type unique = unit ref
  val venv:venv = Env.base_venv
  val tenv:tenv = Env.base_tenv
  val failures = ref 0

    fun checkSameType(ty1: Types.ty, ty2: Types.ty) = case ty1 of  Types.BOTTOM => true
															   | Types.NIL => (case ty2 of Types.RECORD(t,u) => true
																					  | Types.BOTTOM => true
																					  | _ =>false)
																| Types.RECORD(t,u) => (case ty2 of Types.NIL => true
																					  | Types.BOTTOM => true
																					  | _ => ty1=ty2)
																| _ => ty1=ty2 orelse ty2 = Types.BOTTOM
    fun checkLegacy(x: expty, y: expty) = checkSameType(#ty x, #ty y)

    fun handleFail(msg: string): expty = (
        failures := !failures + 1;
        print(msg);
        {exp=TR.handleNil(), ty=Types.BOTTOM})


    fun  checkInt({exp=exp', ty=ty'}: expty, pos, print_) =
        if checkSameType(ty', Types.INT) then true
        else (if print_ then print(Int.toString(pos)^": Error: Expected int token \n") else (); false)

    fun  checkStr({exp=exp', ty=ty'}: expty, pos, print_) =
        if  checkSameType(ty',Types.STRING) then true
        else (if print_ then print(Int.toString(pos)^": Error: Expected string token \n") else (); false)



	fun searchTy(tenv,s,pos) = case Symbol.look(tenv, s) of SOME t => t
													  | NONE   => (#ty (handleFail(Int.toString(pos)^": Error: No such type defined  " ^ Symbol.name s^"\n")))
	fun actual_ty (tenv,ty,pos) = case ty of Types.NAME(s,t) => (case (!t) of NONE => actual_ty (tenv,searchTy(tenv,s,pos),pos)
															| SOME typ => actual_ty (tenv,typ,pos))
											| Types.ARRAY(t,u) => Types.ARRAY(actual_ty (tenv,t,pos),u)
											|  _              => ty
	fun getRecordParam tenv {name=name, escape=escape, typ=typ, pos=pos} = (name, searchTy(tenv,typ,pos))

  fun transExp(venv, tenv, root, lev, breakpoint) =
  let
    datatype OpType = SORT | MATH | EQUALITY
    val equatable = []

    fun getTypeByOperation(A.NeqOp)    = EQUALITY
      | getTypeByOperation(A.EqOp)     = EQUALITY
      | getTypeByOperation(A.PlusOp)   = MATH
      | getTypeByOperation(A.MinusOp)  = MATH
      | getTypeByOperation(A.DivideOp) = MATH
      | getTypeByOperation(A.TimesOp)  = MATH
      | getTypeByOperation(A.GeOp)     = SORT
      | getTypeByOperation(A.GtOp)     = SORT
      | getTypeByOperation(A.LeOp)     = SORT
      | getTypeByOperation(A.LtOp)     = SORT

    and validateMath(left, right, pos, oper) =
        let
            val r = trexp right
            val l = trexp left
        in
            if checkInt(l, pos, true) andalso checkInt(r, pos, true) then {exp=TR.intBinOps(oper, #exp l, #exp r), ty=Types.INT}
                                            else handleFail(Int.toString pos ^ ": Invalid binop expression")
        end

    and validateSort(left, right, pos, oper) =
        let
            val l = trexp left
            val r = trexp right
        in
            if checkLegacy(l, r) andalso checkInt(l, pos, false) then {exp=TR.intBinOps(oper, #exp l, #exp r), ty=Types.INT}
            else ( if  checkLegacy(l, r) andalso checkStr(l, pos, false) then {exp=TR.strBinOps(oper, #exp l, #exp r), ty=Types.INT}
                  else (handleFail(Int.toString(pos)^": Error: Comparison Error: Malformed comparison\n")))
        end

    and validateEquality(left, right, pos, oper) =
      let
        val l: expty = trexp left
        val r: expty = trexp right
      in
        case actual_ty(tenv,(#ty l),pos) of
            Types.INT => if checkInt(r, pos, true) then {exp=TR.intBinOps(oper, #exp l, #exp r), ty=Types.INT} else (print(Int.toString(pos)^": Error: Compared structures must be of same type \n"); {exp=TR.handleNil(), ty=Types.BOTTOM})
          | Types.STRING => if checkStr(r, pos, true) then {exp=TR.strBinOps(oper, #exp l, #exp r), ty=Types.INT} else (print(Int.toString(pos)^": Error: Compared structures must be of same type \n"); {exp=TR.handleNil(), ty=Types.BOTTOM})
          | Types.ARRAY(ty', unique') => if checkSameType(Types.ARRAY(ty', unique'), #ty r) then {exp=TR.intBinOps(oper, #exp l, #exp r), ty=Types.INT} else (print(Int.toString(pos)^": Error: Compared structures must be of same type \n"); {exp=TR.handleNil(), ty=Types.BOTTOM})
          | Types.RECORD(fields, unique') => if checkSameType(Types.RECORD(fields, unique'), #ty r) then {exp=TR.intBinOps(oper, #exp l, #exp r), ty=Types.INT} else (print(Int.toString(pos)^": Error: Compared structures must be of same type \n"); {exp=TR.handleNil(), ty=Types.BOTTOM})
          | Types.NIL => if checkSameType(Types.NIL, #ty r) then {exp=TR.handleInt 1, ty=Types.INT} else (print(Int.toString(pos)^": Error: Compared structures must be of same type \n"); {exp=TR.handleNil(), ty=Types.BOTTOM})
		  | _ => (handleFail(Int.toString(pos)^": Error: Cannont campare structures: can only compare int, string, record, and array types \n"))
      end

    and  trexp(A.IntExp i): expty = {exp=TR.handleInt i, ty=Types.INT}
        |   trexp(A.StringExp (s,pos)) = {exp=TR.handleStr s, ty=Types.STRING}
        |   trexp(A.NilExp) = {exp=TR.handleNil(), ty=Types.NIL}
        |   trexp(A.OpExp{left, oper, right, pos}) = (case getTypeByOperation oper of
                                                      MATH     => validateMath(left, right, pos, oper)
                                                    | SORT     => validateSort(left, right, pos, oper)
                                                    | EQUALITY => validateEquality(left, right, pos, oper)
                                                )
        |   trexp(A.IfExp{test, then', else', pos}) =
                let
                    val then_expty = trexp then'
                    val test_expty = trexp test
                    val else_expty: expty option = if isSome else' then SOME(trexp (valOf else')) else NONE

                    val then_exp = #exp then_expty
                    val test_exp = #exp test_expty
                    val else_exp = if isSome(else_expty) then SOME(#exp (valOf(else_expty))) else NONE
                in
                    if checkInt(test_expty, pos, true) andalso ( (isSome else_expty andalso checkLegacy(then_expty, valOf else_expty)) orelse checkSameType(#ty then_expty, Types.UNIT))
                    then {exp=TR.ifWrapper(test_exp, then_exp, else_exp), ty=(#ty then_expty)}
                    else (handleFail(Int.toString(pos)^": Error: Invalid if conditional statement \n"))
                end
        |   trexp(A.WhileExp{test, body, pos}) =
                let
                    val test' = trexp test
                    val body' = trexp body
                    val breakpoint' = TR.getLabel()
                    val access = TR.allocLocal(lev)(false)

                    val venv' = Symbol.enter(venv,Symbol.symbol "break", Env.VarEntry{ty=Types.INT, access=access, write=false})
                in
                    if checkInt(trexp test, pos, true) andalso (checkSameType(Types.UNIT, #ty (transExp(venv',tenv, body, lev, breakpoint')))) then ({exp=TR.whileExp(#exp test', #exp  body', TR.getLabel()), ty=Types.UNIT})
                                                        else (handleFail(Int.toString(pos)^": Error: While loop construction error \n"))
                end
        |   trexp(A.ForExp{var, lo, hi, body, escape, pos}) =
                let
                    val access = TR.allocLocal lev (!escape)

                    val venv' = Symbol.enter(venv, var, Env.VarEntry({ty=Types.INT, access=access, write=false}))
                    val venv'' = Symbol.enter(venv', Symbol.symbol "break", Env.VarEntry{ty=Types.INT, access=access, write=false})

                    val breakpoint' = TR.getLabel()

                    val {exp=low,   ty=lowTy}  = transExp(venv, tenv, lo, lev, breakpoint')
                    val {exp=high,  ty=highTy} = transExp(venv, tenv, hi, lev, breakpoint')
                    val {exp=body', ty=bodyTy} = transExp(venv'', tenv, body, lev, breakpoint')
                in
                    if checkInt({exp=low, ty=lowTy}, pos, true) andalso checkInt({exp=high, ty=highTy}, pos, true)
                                                                andalso (checkSameType(Types.UNIT, bodyTy))
                                                                then ({exp=TR.forExp(TR.simpleVar(access, lev), TR.getLabel(), low, high, body'), ty=Types.UNIT})
                                                        else ( handleFail(Int.toString(pos)^": Error: For loop construction error \n"))
                end

        |   trexp(A.BreakExp(pos)) = if isSome(Symbol.look(venv,Symbol.symbol "break")) then {exp=TR.breakExp(breakpoint), ty=Types.BOTTOM} else (handleFail(Int.toString(pos)^": Error: Unnested break statement \n"))
        |   trexp(A.VarExp(v)) =
                let
                    val {exp=exp', ty=ty'} = transVar(venv, tenv, v, lev, breakpoint)
                in
                    {exp=exp' , ty=ty'}
                end

        |   trexp(A.AssignExp{var, exp, pos}) =
                let
                    val {exp=ee, ty=expTy} = trexp exp
                    val {exp=e, ty=varTy} = transVar(venv, tenv, var, lev, breakpoint)
					fun getName var = case var of A.SimpleVar(id, pos) => id
												| A.FieldVar(v, id, pos) => getName v
												| A.SubscriptVar(v, exp, pos) => getName v
                in
					case Symbol.look(venv,getName var) of SOME(Env.VarEntry{access,ty,write}) => if write then (
																								if checkSameType(expTy, varTy) then {exp=TR.assign(e, ee), ty = Types.UNIT}
																								else (handleFail(Int.toString(pos)^": Error: Illegal assign expression \n")))
																							else (handleFail(Int.toString(pos)^": Error: For loop id cannot be assigned \n"))
														| SOME(Env.FunEntry _) => handleFail(Int.toString pos ^ ": Cannot assing value to a function\n")
                                                        | _ => handleFail(Int.toString pos ^ ": Undefined variable name " ^ Symbol.name(getName var)^ "\n")
                end
        |   trexp(A.RecordExp{fields, typ, pos}) = (
                case Symbol.look(tenv, typ) of
                    SOME(t) => (case actual_ty(tenv,t,pos) of
                        Types.RECORD(fieldTypes, unique') =>
                        let
                            val reduced = map (fn(sym, {exp, ty}, pos) => {sym=sym, exp = exp, ty=ty, pos=pos}) (map (fn (sym, e, pos) => (sym, trexp e, pos)) fields)
                            val types = map (fn (head) => #ty head) reduced
                            val exps = map(fn(head) => #exp head) reduced
                            val actualTypes = map (fn x => actual_ty(tenv,#2 x,pos)) fieldTypes
							val names = map (fn (head) => #sym head) reduced
                            val actualNames = map (fn x => #1 x) fieldTypes
                            fun f(t1, t2, head) = head andalso checkSameType(t1, t2)
							fun g(t1:Symbol.symbol, t2:Symbol.symbol, head) = head andalso (t1=t2)
                        in
							if (length types) = (length actualTypes) then (
								if ListPair.foldr f true (types, actualTypes)
								then (if ListPair.foldr g true (names, actualNames)
										then {exp=TR.recordExp(exps), ty=Types.RECORD(fieldTypes, unique')}
										else (handleFail(Int.toString(pos)^": Error: Record assignment field name unmatched error \n")))
								else (handleFail(Int.toString(pos)^": Error: Record assignment type error \n")))

							else (handleFail(Int.toString(pos) ^ ": Error: Record error : expected " ^ Int.toString(length fieldTypes) ^ " fields, found " ^ Int.toString(length types)^"\n"))
                        end
                    |   _ => (handleFail(Int.toString(pos)^": Error: Type mismatch in record usage \n")))
                |   NONE    => (handleFail(Int.toString(pos)^": Error: Unknown type " ^ Symbol.name typ ^ "\n"))

            )
        |   trexp(A.SeqExp []) = {exp=TR.handleNil(), ty=Types.UNIT}
        |   trexp(A.SeqExp exps) =
                let
                    fun f ((exp, _), (res, oldTys)) =
                        let
                            val {exp=exp', ty=ty} = transExp(venv, tenv, exp, lev, breakpoint)
                        in
                            (exp' :: res, ty :: oldTys)
                        end
                    val (exps', tys) = foldr f ([], []) exps
                    val lastTy = List.last tys
                in
                    {exp=TR.seqExp exps', ty=lastTy} (*exps' in forward order*)
                end

        |   trexp(A.LetExp{decs, body, pos}) =
                let
                    val {venv=venv',tenv=tenv',exp=ee} = foldl (fn (dec,{venv,tenv,exp}) =>
                        let
                            val {venv=venv1,tenv=tenv1,exp=exp'} = transDec(venv,tenv,dec,lev,breakpoint)
                        in
                            {venv=venv1,tenv=tenv1,exp=exp'::exp}
                        end) {venv=venv, tenv=tenv, exp=[]} decs;
                    val {exp=e,ty=bodyType} = transExp(venv',tenv', body,lev, breakpoint)
                in
                    {exp=TR.letExp(TR.decsPre (rev ee), e), ty=bodyType}
                end
        |   trexp(A.ArrayExp{typ, size, init, pos}) =
                let
                    val size' = trexp size
                    val sizeExp = #exp size'
                    val {exp=init', ty=initTy} = trexp init
                in (case S.look(tenv, typ) of
                        SOME(at) => (
                            case actual_ty(tenv,at,pos) of
                                Types.ARRAY(t, u) =>
                                    if checkInt(size', pos, true) andalso checkSameType(actual_ty(tenv,t,pos), initTy) then {exp=TR.arrayExp(sizeExp, init'), ty=Types.ARRAY(actual_ty(tenv,t,pos),u)}
                                    else (handleFail(Int.toString(pos)^": Error: Invalid array expression "^Symbol.name typ ^" \n"))
                                | _ => (handleFail(Int.toString(pos)^": Error: Type mismatch (should be array type): "^Symbol.name typ ^"\n"))
                            )
                    |   NONE => (handleFail(Int.toString(pos)^": Error: Unknown type \n"))
                )
                end
        |   trexp(A.CallExp{func, args, pos}) = (
                case S.look(venv, func) of
                    SOME(Env.FunEntry{level, label, formals, result}) =>
                    let
                        fun f(ty1, ty2, res) = res andalso checkSameType(ty1, ty2)
                        val args' = map trexp args
						val argTys = map #ty args'
                        val argExps = map #exp args'
                    in
						if (length argTys = length formals) then (
							if (ListPair.foldr f true (formals, argTys)) then {exp=TR.callExp(level, lev, label, argExps, result<>Types.UNIT), ty=result}
							else (handleFail(Int.toString(pos)^": Error: Type disagreement in function arguments \n")))
							else (handleFail(Int.toString(pos) ^": Error: Argument error, expected " ^ Int.toString(length formals) ^ " function arguments, found " ^ Int.toString(length args)^"\n"))

                    end
                |   SOME(Env.VarEntry(_)) => (handleFail(Int.toString(pos)^": Error: Expected a function idenifier, found a variable: pos \n"))
                |   NONE => (handleFail(Int.toString(pos)^": Error: Unknown symbol " ^ Symbol.name func^"\n"))
            )
    in
      trexp(root)
    end

  	and  transTy(tenv,ty) =
     case ty of A.NameTy(s, p) => searchTy(tenv,s,p)
  			  | A.RecordTy(tl) => Types.RECORD(if tl=[] then [] else map (getRecordParam tenv) tl, ref (): Types.unique)
  		      | A.ArrayTy(s,p) => Types.ARRAY(searchTy(tenv,s,p), ref (): Types.unique )
    and transVar (venv, tenv, node, lev, breakpoint): expty =
      let fun trvar (A.SimpleVar(id, pos)) =
  							(case Symbol.look(venv, id)
  							of SOME(Env.VarEntry{access, ty, write}) => {exp = TR.simpleVar(access, lev), ty = actual_ty (tenv,ty,pos)}
                             | SOME(Env.FunEntry(_)) => (print(Int.toString(pos)^": Error: Expected variable symbol, found function : symbol name " ^ Symbol.name id^"\n"); {exp=TR.handleNil(), ty=Types.BOTTOM})
  							 | NONE => (handleFail(Int.toString(pos)^": Error: undefined variable " ^ Symbol.name id^"\n")
  										))
  			  | trvar (A.FieldVar(v, id, pos)) =
							let val {exp=exp', ty=ty} = trvar(v)
							in
								(case ty of Types.RECORD(stl, u) => let fun searchField ((s,t)::m) id = if s = id then actual_ty (tenv,t,pos) else searchField m id
																		  | searchField nil id  = (print(Int.toString(pos)^": Field named" ^ Symbol.name (id) ^ "is not defined in this record\n");
																								   Types.BOTTOM)
																	in
																		{exp = TR.fieldVar(exp', id, map #1 stl), ty = searchField stl id}
																	end
															 | _ => (handleFail(Int.toString(pos)^": Error: Variable is not defined as a record: "^"\n"))
															)
							end

  			  | trvar (A.SubscriptVar(v, exp, pos)) =  (*Do we have to check the bound?*)
							let val {exp=exp', ty=ty} = trvar(v)
  							in
								(case ty of
                                    Types.ARRAY(t, u) =>
                                        let
                                            val translatedOffset = transExp (venv,tenv,exp,lev, breakpoint)
                                            val offsetExp = #exp translatedOffset
                                        in
                                            (if checkInt(translatedOffset, pos, true)
                                                then {exp = TR.subscriptVar(exp', offsetExp ), ty = actual_ty (tenv,t,pos)}
                                                else (handleFail(Int.toString(pos)^": Provided index is not of type int"^"\n")))
                                        end
									| _               => (handleFail(Int.toString(pos)^": Error: Variable is not defined as an array: "^"\n"))
								)
  							end
		in
		trvar node
		end
    and transDec (venv, tenv, A.VarDec{name, escape, typ, init, pos}, lev, breakpoint) =

    (let val {exp = exp, ty = ty} = transExp(venv, tenv, init, lev, breakpoint)
		in
			case ty of Types.NIL => (case typ
								of SOME((s,p)) => (case searchTy (tenv,s,p) of Types.RECORD(tl,u) => let val access = TR.allocLocal(lev)(!escape) in
											{venv = Symbol.enter(venv,name,Env.VarEntry{access=access, ty=Types.RECORD(tl,u),write=true}), tenv=tenv,
											exp = TR.getAssign(access, exp)} end
										| _  => (handleFail(Int.toString(pos)^": Error: Initializing nil expressions not constrained by record type: " ^ Symbol.name name^"\n");
											{venv=venv,tenv=tenv, exp= TR.handleNil()}))
								 | NONE =>
									(handleFail(Int.toString(pos)^": Error: Initializing nil expressions not constrained by record type: " ^ Symbol.name name^"\n");
										{venv=venv,tenv=tenv, exp= TR.handleNil()}))
					| _ =>  let val access = TR.allocLocal(lev)(!escape) in
							(case typ
								of SOME((s,p)) => if checkSameType(actual_ty(tenv,searchTy(tenv,s,p),p), ty)
														then {venv=Symbol.enter(venv,name,Env.VarEntry{access=access, ty=ty,write=true}), tenv=tenv,
														exp = TR.getAssign(access, exp)}
														else (handleFail(Int.toString(pos)^": Error: Unmatched defined variable type " ^ Symbol.name name^"\n");
															  {venv=venv,tenv=tenv, exp= TR.handleNil()})
								 | NONE =>
									{venv=Symbol.enter(venv,name,Env.VarEntry{access=access,ty=ty,write=true}), tenv=tenv,
									exp= TR.getAssign(access, exp)}) end

		end
		)

	  | transDec (venv, tenv, A.TypeDec(l), lev, breakpoint) =
			let
				fun redefineCheck (s,{namemap=namemap,nameset=nameset}) = {namemap=mymap.insert(namemap,s,set.member(nameset,s)),nameset= set.add(nameset,s)}
				val {namemap=namemap,nameset=nameset} = foldl redefineCheck {namemap=mymap.empty,nameset=set.empty} (map #name l)
				val tenv' = foldl (fn (a,tenv) => if valOf(mymap.find(namemap,#name a)) then (handleFail(Int.toString(#pos a)^": Error: Type redifined " ^ Symbol.name (#name a)^"\n");
																								Symbol.enter(tenv,#name a,Types.BOTTOM))
																else Symbol.enter(tenv,#name a,Types.NAME(#name a, ref NONE))) tenv l
				val l' = map (fn a => if valOf(mymap.find(namemap,#name a)) then (#name a,Types.BOTTOM,#pos a) else (#name a, transTy(tenv',#ty a),#pos a)) l
				val tenv''=foldl (fn (a, tenv) => if valOf(mymap.find(namemap,#1 a)) then tenv else Symbol.enter(tenv,#1 a,#2 a)) tenv' l'
				fun getRidOfCycle (a,(ty,pos,visited),tenv)= (case ty of Types.NAME(s,t) => (case (!t) of NONE => (if set.member(visited,s)
																										then (#ty (handleFail(Int.toString(pos)^": Error: Type decs deadlock " ^ Symbol.name a^"\n")))
																										else getRidOfCycle(a,(searchTy(tenv,s,pos),pos,set.add(visited,s)),tenv))
																						| SOME typ => getRidOfCycle (a,(typ,pos,set.add(visited,s)),tenv))
																| _ => ty)

				val ht = foldl (fn (a,b) => (#1 a,getRidOfCycle(#1 a,(#2 a, #3 a, set.add(set.empty,#1 a)),tenv''))::b) [] l'
			in
				{venv=venv,
				 tenv=(foldl (fn (a, tenv) => Symbol.enter(tenv,#1 a,#2 a)) tenv'' ht),
				 exp=TR.handleNil()}
			end

	  | transDec (venv, tenv, A.FunctionDec(l), lev, breakpoint) =
		let
			fun redefineCheck (s,{namemap=namemap,nameset=nameset}) = {namemap=mymap.insert(namemap,s,set.member(nameset,s)),nameset= set.add(nameset,s)}
			val {namemap=namemap,nameset=nameset} = foldl redefineCheck {namemap=mymap.empty,nameset=set.empty} (map #name l)
			fun passHeader ({name,params,body,pos,result}, {venv=venv,tenv=tenv}) =
				if valOf(mymap.find(namemap,name)) then (handleFail(Int.toString(pos)^": Error: Function name redefined " ^ Symbol.name name^"\n");{venv=venv,tenv=tenv})
				else (
					let
						val result_ty = valOf(case result of SOME(rt,pos) => (case Symbol.look(tenv,rt) of SOME(t) => SOME(t)
																										 | NONE => (SOME (#ty (handleFail(Int.toString(pos)^": Error: Undefined return type " ^ Symbol.name rt^"\n")))))

														   | NONE => SOME Types.UNIT)
						fun transparam {name, escape, typ, pos} =
												case Symbol.look(tenv,typ)
													of SOME t => {name=name, ty=t}
													| NONE => (handleFail(Int.toString(pos)^": Error: Undefined parameter type " ^ Symbol.name name^"\n");
													  {name = name, ty = Types.BOTTOM})
						val params' = map transparam params
						val esc = map (fn x => !(#escape x)) params
						val venv' = Symbol.enter(venv,name,Env.FunEntry{level=TR.newLevel({parent=lev, name=name, formals=esc}), label= name, formals = map #ty params', result=result_ty})
					in
						{venv=venv',tenv=tenv}
					end
				)
			val {venv=venv'',tenv=tenv} = foldl passHeader {venv=venv,tenv=tenv} l

			fun oneFunc ({name,params,body,pos,result}, {venv=venv,tenv=tenv,exp=exp}) =
                let
                    val SOME(Env.FunEntry{level=funLev, label, formals, result=result'}) = Symbol.look(venv,name)
                in
    				if valOf(mymap.find(namemap,name)) then {venv=venv,tenv=tenv,exp=TR.handleNil()}
    				else (
    					let
    						val result_ty = (case result of SOME(rt,pos) => (case Symbol.look(tenv,rt) of SOME(t) => t
    																										 | NONE => Types.BOTTOM)
    														   | NONE => Types.UNIT)
    						val (formals, levv) = case valOf(Symbol.look(venv'',name)) of Env.FunEntry({level, label, formals, result}) => (TR.getFormals(level), level) (* SML has a type issue here since not all Env.enventry have levels => type unsafe *)
    						fun transparam ({name, escape, typ, pos}, access) =(
    												case Symbol.look(tenv,typ)
    													of SOME t => {name=name, access=access, ty=t}
    													| NONE =>  {name = name, access=access, ty = Types.BOTTOM})
    						val params' = ListPair.map transparam (params,formals)

    						fun enterparam ({name=name,access=access, ty=ty}, venv) =
    									Symbol.enter(venv,name,Env.VarEntry{access =access ,ty=ty, write=true})
    						val venv''' = foldl enterparam venv params'
							val {exp=expp, ty=ty'} = transExp(venv''',tenv, body, funLev, breakpoint)
							
    					in
    						if checkLegacy({exp=expp, ty=ty'}, {exp=TR.handleNil(), ty=result_ty})
    										then (TR.procEntryExit {level = levv, body = expp}; {venv=venv,tenv=tenv,exp=TR.handleNil()})
    										else  ( handleFail(Int.toString(pos)^": Error: return type do not match " ^ Symbol.name name^"\n");
													TR.procEntryExit{level = levv, body = expp};
    												{venv=venv,tenv=tenv,exp=TR.handleNil()})

    					end
    				)
                end
		in
			foldl oneFunc {venv=venv'',tenv=tenv,exp=TR.handleNil()} l
		end

    fun transProg(root) =
        let
            val mainLevel = TR.newLevel({parent=TR.root, name=TR.namedlabel "main", formals=[]})
            val translated = transExp(venv, tenv, root, mainLevel, TR.namedlabel "main")
			val _ = TR.procEntryExit{level = mainLevel, body = #exp translated};
            val failures' = !failures
        in
            Printtree.printtree(TextIO.stdOut, TR.unNx(#exp(translated)));
            failures := 0;
            if failures' >0 then print("Compilation failed with " ^ Int.toString (failures') ^ " errors\n") else ();
			TR.getResult()

        end

end
