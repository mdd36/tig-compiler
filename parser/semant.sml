signature Semant =
sig
	type expty = {exp: Translate.exp, ty: Types.ty}
	type venv = Env.enventry Symbol.table
	type tenv = ty Symbol.table
	
	transVar: venv * tenv * Absyn.var -> expty
	transExp: venv * tenv * Absyn.exp -> expty
	transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
	transTy:         tenv * Absyn.dec -> Types.ty
	
end

structure Semant :> Semant
	structure A = Absyn
	val venv = Env.base_venv
	val tenv = Env.base_tenv
	fun actual_ty ty = case ty of Types.NAME(s,t) => !t
								 |                => ty 
							 
	fun transTy(tenv,ty) = 	
		let 
			fun firsttrans t = case t of A.NameTy(s, p) =>  Types.NAME(s, ty option ref)
									   | A.RecordTy of field list
									   | A.ArrayTy(s,p) => Types.ARRAY(firsttrans s, Types.unique)
									   
	fun transVar (venv, tenv) =
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
							
			  | trvar (A.SubscriptVar(v, exp, pos)) = 
	
	fun transDec (venv, tenv, A.VarDec{name, escape, typ = NONE, init, pos}) =
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
			
		end
	
	fun transProg
	
	


end