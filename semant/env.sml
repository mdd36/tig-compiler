structure S = Symbol

signature ENV =
sig
  type ty
  datatype enventry = VarEntry of {access: Translate.access, ty: ty,write:bool}
                    | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: ty}
  val base_tenv: ty Symbol.table
  val base_venv: enventry Symbol.table
end

structure Env : ENV =
struct
  type ty = Types.ty

  datatype enventry = VarEntry of {access: Translate.access, ty: ty,write:bool}
                    | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: ty}

  val base_tenv = S.enter(
                    S.enter(
                        S.empty, S.symbol("string"), Types.STRING
                        ), S.symbol("int"), Types.INT
                    )

  val base_funs = [
    {name="print", details=FunEntry{level=Translate.root, label=Temp.namedlabel "print", formals=[Types.STRING], result=Types.UNIT}},
    {name="flush", details=FunEntry{level=Translate.root, label=Temp.namedlabel "flush", formals=[], result=Types.UNIT}},
    {name="getchar", details=FunEntry{level=Translate.root, label=Temp.namedlabel "getchar", formals=[], result=Types.STRING}},
    {name="ord", details=FunEntry{level=Translate.root, label=Temp.namedlabel "ord", formals=[Types.STRING], result=Types.INT}},
    {name="chr", details=FunEntry{level=Translate.root, label=Temp.namedlabel "chr", formals=[Types.INT], result=Types.STRING}},
    {name="size", details=FunEntry{level=Translate.root, label=Temp.namedlabel "size", formals=[Types.STRING], result=Types.INT}},
    {name="substring", details=FunEntry{level=Translate.root, label=Temp.namedlabel "substring", formals=[Types.STRING, Types.INT, Types.INT], result=Types.STRING}},
    {name="concat", details=FunEntry{level=Translate.root, label=Temp.namedlabel "concat", formals=[Types.STRING, Types.STRING], result=Types.STRING}},
    {name="not", details=FunEntry{level=Translate.root, label=Temp.namedlabel "not", formals=[Types.INT], result=Types.INT}},
    {name="exit", details=FunEntry{level=Translate.root, label=Temp.namedlabel "exit", formals=[Types.INT], result=Types.UNIT}}
  ]


  val base_venv = foldr (fn (head, res) => S.enter(res, S.symbol(#name head), #details head)) S.empty base_funs
end
