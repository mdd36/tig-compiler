structure A = Absyn

structure Semant :
  sig
    type expty = {exp: Translate.exp, ty: Types.ty}
    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    val transVar: (venv * tenv * A.var) -> expty
    val transExp: venv * tenv * A.exp -> expty
    val transDec: venv * tenv * A.dec -> {venv: venv, tenv: tenv}
    val transTy :        tenv * A.ty  -> Types.ty
  end =
struct

  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table

  val venv = Env.base_venv
  val tenv = Env.base_tenv

  fun transProg() = () (*TODO*)

  (*fun checkSameType({_, ty=ty1}, {_, ty=ty2}) =
      if (ty1 <> ty2) then
          (case (ty1, ty2) of
              (_, Types.BOTTOM) => true
            | (Types.BOTTOM, _) => true
            | (_,_) => false
          )
      else *)
  fun checkSameType(ty1: expty, ty2: expty) = ty1 = ty1 orelse ty1 = Types.BOTTOM orelse ty2 = Types.BOTTOM
  fun checkLegacy(x, y) = checkSameType(#ty x, #ty y)


  fun  checkInt({exp', ty'}, pos, print_) =
      if checkSameType(ty', Types.INT) then true
      else (if print_ then print("Error: Expected int token at " ^ Int.toString(pos)) else (); false)


    fun  checkStr({exp', ty'}, pos, print_) =
        if  checkSameType(ty',Types.STRING) then true
        else (if print_ then print("Error: Expected string token at " ^ Int.toString(pos)) else (); false)

  fun transExp(venv, tenv, root) =
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

    and validateMath(left, right, pos) = if checkInt(trexp(left), pos, true) andalso checkInt(trexp(right), pos, true) then {exp=(), ty=Types.INT}
                                    else {exp=(), ty=Types.BOTTOM}
    and validateSort(left, right, pos) =
        let
            val l = trexp left
            val r = trexp right
        in
            if checkLegacy(l, r) andalso (checkInt(l, pos, false) orelse checkStr(l, pos, false)) then {exp=(), ty=Types.INT}
            else (print("Comparison Error: Malformed comparison at " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
        end

    and validateEquality(left, right, pos) =
      let
        val l = trexp left
        val r = trexp right
      in
        case l of
            Types.INT => if checkInt(r, pos, true) then {exp=(), ty=Types.INT} else (print("Compared structures must be of same type: pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
          | Types.STRING => if checkStr(r, pos, true) then {exp=(), ty=Types.INT} else (print("Compared structures must be of same type: pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
          | Types.ARRAY(ty', unique') => if checkSameType(Types.ARRAY(ty', unique'), #ty r) then {exp=(), ty=Types.INT} else (print("Compared structures must be of same type: pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
          | Types.RECORD(fields, unique') => if checkSameType(Types.RECORD(fields, unique'), #ty r) then {exp=(), ty=Types.INT} else (print("Compared structures must be of same type: pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
          | _ => (print("Cannont camparer structures at pos " ^ Int.toString(pos) ^ ": can only compare int, string, record, and array types"); {exp=(), ty=Types.BOTTOM})
      end

    and  trexp(A.IntExp i) = {exp=(), ty=Types.INT}
        |   trexp(A.StringExp (s,pos)) = {exp=(), ty=Types.STRING}
        |   trexp(A.NilExp) = {exp=(), ty=Types.UNIT}
        |   trexp(A.OpExp{left, oper, right, pos}) = (case getTypeByOperation oper of
                                                      MATH     => validateMath(left, right, pos)
                                                    | SORT     => validateSort(left, right, pos)
                                                    | EQUALITY => validateEquality(left, right, pos)
                                                    | _        => {exp=(), ty=Types.BOTTOM}
                                                )
        |   trexp(A.IfExp{test, then', else', pos}) =
                let
                    val expty' = trexp then'
                in
                    if checkInt(trexp test, pos) andalso checkLegacy(expty',  trexp(getOpt(else', A.NilExp))) then {exp=(), ty=(#ty expty')}
                    else (print("Error: Invalid if conditional statement at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
                end
        |   trexp(A.WhileExp{test, body, pos}) =  if checkInt(trexp test, pos) andalso checkSameType(Types.UNIT, #ty (trexp body)) then {exp=(), ty=Types.UNIT}
                                                else (print("Error: While loop construction error at " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
        |   trexp(A.ForExp{var, lo, hi, body, escape, pos}) = if checkInt(trexp lo, pos) andalso checkInt(trexp hi, pos) andalso checkSameType(Types.UNIT, #ty (trexp body)) then {exp=(), ty=Types.UNIT}
                                                    else (print("Error: For loop construction error at " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
        |   trexp(A.BreakExp(_)) = {exp=(), ty=Types.BOTTOM}
        |   trexp(A.VarExp(v)) = {exp=(), ty=(trvar v)}
        |   trexp(A.AssignExp{var, exp, pos}) =
                let
                    val expTy = trexp exp
                    val varTy = trvar var
                in
                    if checkLegacy(expTy, varTy) then {exp=(), ty = #ty expTy}
                    else (print("Illegal assign expression at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
                end
        |   trexp(A.RecordExp{fields, typ, pos}) = (
                case Symbol.look(tenv, typ) of
                    SOME(t) => case actual_ty(t, pos) of
                        Types.RECORD(fieldTypes, unique') =>
                        let
                            fun getFieldTypes({exp, ty}) = () (*TODO*)
                            fun validateRecord() = ()
                        in
                            {exp=(), ty=Types.BOTTOM}(*TODO*)
                        end
                    |   _ => (print("Type mismatch in record usage at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
                |   NONE    => (print("Unknown type " ^ Symbol.name typ ^ " at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})

            )
        |   trexp(A.SeqExp(exps)) =
                if exp <> [] then {exp=(), ty=(#ty trexp (#1 (List.last exps)))} else {exp=(), ty=Types.UNIT}

        |   trexp(A.LetExp{declarations, body, pos}) =
                let
                    val {venv', tenv'} = foldr (fn(declaration, {venv, tenv}) => {venv=venv1, tenv=tenv1} = transDec(venv, tenv, declaration)) {venv=venv, tenv=tenv} declarations
                    val {exp=_, ty=bodyType} = transExp(venv', tenv', body)
                in
                    {exp=(), ty=bodyType}
                end
        |   trexp(A.ArrayExp{typ, size, init, pos}) = (
                case S.look(tenv, typ) of
                    SOME(at) => (
                        case actual_ty(at) of
                            Types.ARRAY(t, u) =>
                                if checkInt(trexp size, pos, true) andalso checkSameType(at, #ty (trexp init)) then {exp=(), ty=Types.ARRAY(t,u)}
                                else (print("Invalid array expression at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
                            | _ => (print("Type mismatch at array exp at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
                        )
                |   NONE => (print("Unknown type at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
            )
        |   trexp(A.CallExp{func, args, pos}) = (
                case S.look(venv, func) of
                    SOME(Env.FunEntry{formals, result}) => if (foldr (fn(res,head) => res andalso (*TODO*)#1 head andalso #2 head) true ListPair.zip(formals, args)) then {exp=(), ty=result}
                                                           else (print("Type disagreement in function arguments at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
                                                           handle ListPair.UnequalLengths =>    (print("Argument error at pos " ^ Int.toString(pos) ^ ", expected " ^ Int.toString(length formals) ^ " function arguments, found " ^ Int.toString(length args));
                                                                                       {exp=(), ty=Types.BOTTOM})
                |   SOME(_) => (print("Expected a function idenifier, found a variable: pos " ^ Int.toString(pos));{exp=() ,ty=Types.BOTTOM})
                |   NONE => (print("Unknown symbol at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})

            )
        and trvar (_) = {exp=(), ty=Types.BOTTOM}
        and actual_ty(_) = Types.BOTTOM
    in
      trexp(root)
    end

    and transDec (venv, tenv, A.VarDec{name, escape, typ = NONE, init, pos}) = {venv=venv, tenv=tenv}
		(* Commenting this out to test for compilation. TODO
    (let val {exp, ty} = transExp {venv, tenv, init}
		in
		case typ
		  of NONE =>
				{tenv=tenv,
				 venv=Symbol.enter(venv,name,Env.VarEntry{ty=ty})}

	    | of SOME(Env.VarEntry{ty}) => if checkSameType({exp=(), Symbol.look(tenv,ty)}, {exp, ty})
									then {tenv=tenv,
										  venv=Symbol.enter(venv,name,Env.VarEntry{ty=ty})}
									else (error pos ("Unmatched defined variable type "^Symbol.name name);
										  {exp = (), ty = Types.BOTTOM})
		end
		)

	  | transDec (venv, tenv, A.TypeDec[{name,ty}]) =
			{venv=venv,
			 tenv=Symbol.enter(tenv,name,transTy(tenv,ty))}

	  | transDec (venv, tenv, A.FunctionDec[{name,params,body,pos,result}]) =
		let
			val SOME(result_ty) = case result of SOME(rt,pos) => Symbol.look(tenv,rt)
											| of NONE => SOME Types.UNIT
			fun transparam{name, typ, pos} =
									case Symbol.look(tenv,typ)
										of SOME t => {name=name, ty=t}
									  | of NONE => (error pos ("Undefined parameter type "^Symbol.name name);
										  {name = name, ty = Types.BOTTOM})
			val params' = map transparam params
			val venv' = Symbol.enter(venv,name,Env.FunEntry{formals = map #ty params', result=result_ty})
			fun enterparam ({name, ty}, venv) =
						Symbol.enter(venv,name,Env.VarEntry{access=(),ty=ty})
			val venv'' = fold enterparam params' venv'
		in
			if checkSameType(transExp(venv'',tenv) body, {_, result_ty})
							then {venv=venv',tenv=tenv}
							else  ( error pos ("return type do not match "^Symbol.name name);
									{venv=venv',tenv=tenv})

		end*)

    (* This is type mismatch -- t is a ty option ref,
    so the case where it's none isn't handled. I've
    commented it out for now so I can test other things.
  	fun actual_ty ty = case ty of Types.NAME(s,t) => !t
  								 |  _              => ty *)

  	and transTy(tenv,ty) = Types.BOTTOM
  		(* TODO uncomment this and actually make it compile
      let
  			fun firsttrans t = case t of A.NameTy(s, p) =>  Types.NAME(s, ty option ref)
  									   | A.RecordTy of field list
  									   | A.ArrayTy(s,p) => Types.ARRAY(firsttrans s, Types.unique)
                       *)
  	and transVar (venv, tenv, node) = {exp=(), ty=Types.BOTTOM}
  		(*TODO uncomment this and actually make it compile
      let fun trvar (A.SimpleVar(id, pos)) =
  							(case Symbol.look(venv, id)
  							of SOME(Env.VarEntry{ty}) => {exp = (), ty = actual_ty ty}
  							 | NONE => (error pos ("undefined variable "^Symbol.name id);
  										{exp = (), ty = Types.BOTTOM})
  			  | trvar (A.FieldVar(v, id, pos)) =
  							(case Symbol.look(venv, id)
  							of SOME(Env.VarEntry{ty}) => {exp = (), ty = actual_ty ty}
  							 | NONE => (error pos ("undefined variable "^Symbol.name id);
  										{exp = (), ty = Types.BOTTOM})

  			  | trvar (A.SubscriptVar(v, exp, pos)) = () *)

end
