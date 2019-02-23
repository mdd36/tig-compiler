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
  type unique = unit ref
  val venv:venv = Env.base_venv
  val tenv:tenv = Env.base_tenv

  fun transProg() = () (*TODO*)

  fun  checkInt({exp, ty}, pos) = (case ty of
        Types.INT => true
     |  Types.BOTTOM => true
     |  _ => (print("Error: Expected string token at " ^ Int.toString(pos)); false))

  fun  checkStr({exp, ty}, pos) = (case ty of
       Types.STRING => true
     | Types.BOTTOM => true
     | _ => (print("Error: Expected string token at " ^ Int.toString(pos)); false))

  (*fun compareRecList([], []) = true
    | compareRecList(a1::s1, a2::s2) = checkSameType(a1, a2) andalso compareRecList(s1, s2)
    | compareRecList(_, _) = false*)

  fun checkSameType(e1:expty, e2:expty) = (#ty e1 = #ty e2 orelse #ty e1 = Types.BOTTOM orelse #ty e2 = Types.BOTTOM)
  (*|checkSameType({_, Types.ARRAY a1}, {_, Types.ARRAY a2}) = checkSameType(#1 a1, #1 a2)*)
	(* This is type mismatch -- t is a ty option ref,
    so the case where it's none isn't handled. I've
    commented it out for now so I can test other things.*)
  	fun actual_ty ty = case ty of Types.NAME(s,t) => actual_ty (valOf (!t))
  								 |  _              => ty 
	fun searchTy(tenv,s,pos) = case Symbol.look(tenv, s) of SOME t => actual_ty t
													  | NONE   => (print(Int.toString(pos)^"Error: No such type defined  " ^ Symbol.name s);
																	Types.BOTTOM)
	fun getRecordParam tenv {name=name, escape=escape, typ=typ, pos=pos} = (name, searchTy(tenv,typ,pos)) 
  	fun transTy(tenv,ty) = 
  		(* TODO uncomment this and actually make it compile*)
     case ty of A.NameTy(s, p) => Types.NAME(s, ref (SOME (searchTy(tenv,s,p))))																					
  			  | A.RecordTy(tl) => Types.RECORD(if tl=[] then [] else map (getRecordParam tenv) tl, ref (): Types.unique)
  		      | A.ArrayTy(s,p) => Types.ARRAY(searchTy(tenv,s,p), ref (): Types.unique )
			  
			  
  fun transVar (venv, tenv, node) = 
  		(*TODO uncomment this and actually make it compile*)
      let fun trvar (A.SimpleVar(id, pos)) =
  							(case Symbol.look(venv, id)
  							of SOME(Env.VarEntry{ty}) => {exp = (), ty = actual_ty ty}
  							 | NONE => (print(Int.toString(pos)^"Error: undefined variable " ^ Symbol.name id);
  										{exp = (), ty = Types.BOTTOM}))
  			  | trvar (A.FieldVar(v, id, pos)) =
  							(case Symbol.look(venv, id)
  							of SOME(Env.VarEntry{ty}) => {exp = (), ty = actual_ty ty}
  							 | NONE => (print(Int.toString(pos)^"Error: undefined variable " ^ Symbol.name id);
  										{exp = (), ty = Types.BOTTOM}))

  			  (*| trvar (A.SubscriptVar(v, exp, pos)) = 
							(case Symbol.look(venv, id)
  							of SOME(Env.VarEntry{ty}) => {exp = (), ty = actual_ty ty}
  							 | NONE => (print(Int.toString(pos)^"Error: undefined variable " ^ Symbol.name id);
  										{exp = (), ty = Types.BOTTOM}))*)
		in
		trvar node
		end
			  
			  
  fun transExp(venv, tenv, root) =
  let
  (*|   trexp(SOME x) = trexp(x) Not sure about this*)
  fun   trexp(A.IntExp i) = {exp=(), ty=Types.INT}
    |   trexp(A.StringExp (s,pos)) = {exp=(), ty=Types.STRING}
    |   trexp(A.NilExp) = {exp=(), ty=Types.NIL} (*I changed the UNIT to NIL*)
    |   trexp(A.OpExp{left, oper=A.PlusOp, right, pos}) =
          if checkInt(trexp(left), pos) andalso checkInt(trexp(right), pos) then {exp=(), ty=Types.INT}
          else {exp=(), ty=Types.BOTTOM}  (*print error messages?*)
    |   trexp(A.OpExp{left, oper=A.MinusOp, right, pos}) =
          if checkInt(trexp(left), pos) andalso checkInt(trexp(right), pos) then {exp=(), ty=Types.INT}
          else {exp=(), ty=Types.BOTTOM}
    |   trexp(A.OpExp{left, oper=A.DivideOp, right, pos}) =
          if checkInt(trexp(left), pos) andalso checkInt(trexp(right), pos) then {exp=(), ty=Types.INT}
          else {exp=(), ty=Types.BOTTOM}
    |   trexp(A.OpExp{left, oper=A.TimesOp, right, pos}) =
          if checkInt(trexp(left), pos) andalso checkInt(trexp(right), pos) then {exp=(), ty=Types.INT}
          else {exp=(), ty=Types.BOTTOM}
    |   trexp(A.IfExp{test, then', else', pos}) =
        let
          val expty' = trexp then'
        in
          if checkInt(trexp test, pos) andalso checkSameType(expty',  trexp(getOpt(else', A.NilExp))) then {exp=(), ty=(#ty expty')}
          else (print("Error: Invalid if conditional statement at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
        end
    (*TODO how to think about comparison ops? Just that they must be the same type and map to int?*)
    |   trexp(A.WhileExp{test, body, pos}) = if checkInt(trexp test, pos) andalso checkSameType({exp=(), ty=Types.UNIT}, trexp body) then {exp=(), ty=Types.UNIT}
                                       else (print("Error: While loop construction error at " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
    |   trexp(A.ForExp{var, lo, hi, body, escape, pos}) = if checkInt(trexp lo, pos) andalso checkInt(trexp hi, pos) andalso checkSameType({exp=(), ty=Types.UNIT}, trexp body) then {exp=(), ty=Types.UNIT}
                                                  else (print("Error: For loop construction error at " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
	(*|   trexp(A.RecordExp{fields, typ, pos}) = *)
  in
    trexp(root)
  end

  
  fun transDec (venv, tenv, A.VarDec{name, escape, typ, init, pos}) = 
		
    (let val {exp = exp, ty = ty} = transExp(venv, tenv, init)
		in
		case typ
			of SOME((s,p)) => if checkSameType({exp=(), ty=searchTy (tenv,s,p)}, {exp=exp, ty=ty})
									then {venv=Symbol.enter(venv,name,Env.VarEntry{ty=ty}), tenv=tenv}
									else (print(Int.toString(pos)^"Error: Unmatched defined variable type " ^ Symbol.name name);
										  {venv=venv,tenv=tenv})
		     | NONE =>
				{venv=Symbol.enter(venv,name,Env.VarEntry{ty=ty}), tenv=tenv}

		end
		)

	  | transDec (venv, tenv, A.TypeDec[{name,ty,pos}]) =
			{venv=venv,
			 tenv=Symbol.enter(tenv,name,transTy(tenv,ty))}

	  | transDec (venv, tenv, A.FunctionDec[{name,params,body,pos,result}]) =
		let
			val SOME(result_ty) = case result of SOME(rt,pos) => Symbol.look(tenv,rt)
											   | NONE => SOME Types.UNIT
			fun transparam {name, escape, typ, pos} =
									case Symbol.look(tenv,typ)
										of SOME t => {name=name, ty=t}
									     | NONE => (print(Int.toString(pos)^"Error: Undefined parameter type " ^ Symbol.name name);
										  {name = name, ty = Types.BOTTOM})
			val params' = map transparam params
			val venv' = Symbol.enter(venv,name,Env.FunEntry{formals = map #ty params', result=result_ty})
			fun enterparam ({name=name, ty=ty}, venv) =
						Symbol.enter(venv,name,Env.VarEntry{ty=ty})
			val venv'' = foldl enterparam venv' params'
		in
			if checkSameType(transExp(venv'',tenv, body), {exp=(), ty=result_ty})
							then {venv=venv',tenv=tenv}
							else  ( print(Int.toString(pos)^"Error: return type do not match " ^ Symbol.name name);
									{venv=venv',tenv=tenv})

		end   
               
end
