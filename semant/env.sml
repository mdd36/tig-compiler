structure S = Symbol

signature ENV =
sig
  type access
  type ty
  datatype enventry = VarEntry of {access: Translate.access, ty: ty,write:bool}
                    | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: ty}
  val base_tenv: ty Symbol.table
  val base_venv: enventry Symbol.table
end

structure Env : ENV =
struct
  type ty = Types.ty

  datatype enventry = VarEntry of {ty: ty,write:bool}
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


  val base_venv = foldr (fn (head, res) => S.enter(res, S.symbol(#name head), #details head)) S.empty base_funs
end
