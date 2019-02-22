structure S = Symbol

signature ENV =
sig
  (*type access*)
  type ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
  val base_tenv: ty Symbol.table
  val base_venv: enventry Symbol.table
end

structure Env : ENV =
struct
  type ty = Types.ty

  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}

  val base_tenv = S.enter(
                    S.enter(
                        S.empty, S.symbol("string"), Types.STRING
                        ), S.symbol("int"), Types.INT
                    )
                    
  val base_funs = [
    {name="print", details=FunEntry{formals=[Types.STRING], result=Types.UNIT}},
    {name="flush", details=FunEntry{formals=[], result=Types.UNIT}},
    {name="getchar", details=FunEntry{formals=[], result=Types.STRING}},
    {name="ord", details=FunEntry{formals=[Types.STRING], result=Types.INT}},
    {name="chr", details=FunEntry{formals=[Types.INT], result=Types.STRING}},
    {name="size", details=FunEntry{formals=[Types.STRING], result=Types.INT}},
    {name="substring", details=FunEntry{formals=[Types.STRING, Types.INT, Types.INT], result=Types.STRING}},
    {name="concat", details=FunEntry{formals=[Types.STRING, Types.STRING], result=Types.STRING}},
    {name="not", details=FunEntry{formals=[Types.INT], result=Types.INT}},
    {name="exit", details=FunEntry{formals=[Types.INT], result=Types.UNIT}}
  ]

  fun base_venv_helper(head: {name:string, details:enventry}, res) =
    let
      val name = S.symbol(#name head)
      val details = #details head
    in
      S.enter(res, name, details)
    end


  val base_venv = foldr base_venv_helper S.empty base_funs
end
