signature Env =
sig
  type access
  type ty
  datatype enventry = VarEntry of {ty: ty}
  	   	    | FunEntry of {formals: ty list, result: ty}
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

structure Env :> Env =
struct 
  type ty = Types.ty
  val base_tenv : ty Symbol.table = Symbol.empty
  val base_venv : enventry Symbol.table = Symbol.empty
end