structure A = Absyn

signature SEMANT =
sig
  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table


  transVar: venv * tenv * A.var -> expty
  transExp: venv * tenv * A.exp -> expty
  transDec: venv * tenv * A.dec -> {venv: venv, tenv: tenv}
  transTy :        tenv * A.ty  -> Types.ty

  checkInt: (A.exp * Types.ty) * int -> unit

end

structure Semant :> SEMANT=
struct

  fun assert(exp, msg) = if exp then () else raise msg

  fun transProg = (); (*TODO*)

  fun  checkInt({exp, Types.INT   }, pos) = true
     | checkstr({exp, Types.BOTTOM}, pos) = true
     | checkstr({exp, _           }, pos) = (print("Error: Expected string token at " ^ Integer.toString(pos)); false)

  fun  checkStr({exp, Types.STRING}, pos) = true
     | checkstr({exp, Types.BOTTOM}, pos) = true
     | checkstr({exp, _           }, pos) = (print("Error: Expected string token at " ^ Integer.toString(pos)); false)

  (*fun compareRecList([], []) = true
    | compareRecList(a1::s1, a2::s2) = checkSameType(a1, a2) andalso compareRecList(s1, s2)
    | compareRecList(_, _) = false*)

  fun checkSameType({_, Types.NIL}, {_, Types.NIL}) = true
    | checkSameType({_, Types.INT}, {_, Types.INT}) = true
    | checkSameType({_, Types.SRING}, {_, Types.STRING}) = true
    | checkSameType({_,  _}, {_, Types.BOTTOM}) = true
    | checkSameType({_, Types.BOTTOM}, {_, _}) = true
    | checkSameType({_, Types.UNIT}, {_, Types.UNIT}) = true
    | checkSameType(_, _) = false
  (*|checkSameType({_, Types.ARRAY a1}, {_, Types.ARRAY a2}) = checkSameType(#1 a1, #1 a2)*)

  fun transExp(venv, tenv, root) =
  let
    fun trexp(NONE) = {exp=(), ty=Types.UNIT} (*Not sure about this*)
    |   trexp(SOME x) = trexp(x)
    |   trexp(A.IntExp) = {exp=(), ty=Types.INT}
    |   trexp(A.StringExp) = {exp=(), ty=Types.STRING}
    |   trexp(A.NilExp) = {exp=(), ty=Types.UNIT}
    |   trexp(A.OpExp{left, oper=A.PlusOp, right, pos}) =
          if checkInt(trexp left, pos) andalso checkInt(trexp right, pos) then {exp=(), ty=Types.INT})
          else (exp=(), tp=Types.BOTTOM)
    |   trexp(A.OpExp{left, oper=A.MinusOp, right, pos}) =
          if checkInt(trexp left, pos) andalso checkInt(trexp right, pos) then {exp=(), ty=Types.INT})
          else (exp=(), tp=Types.BOTTOM)
    |   trexp(A.OpExp{left, oper=A.DivideOp, right, pos}) =
          if checkInt(trexp left, pos) andalso checkInt(trexp right, pos) then {exp=(), ty=Types.INT})
          else (exp=(), tp=Types.BOTTOM)
    |   trexp(A.OpExp{left, oper=A.TimesOp, right, pos}) =
          if checkInt(trexp left, pos) andalso checkInt(trexp right, pos) then {exp=(), ty=Types.INT})
          else (exp=(), tp=Types.BOTTOM)
    |   trexp(A.IfExp{test, then', else', pos}) =
        let
          val ty' = trexp then'
        in
          if checkInt(trexp test, pos) andalso checkSameType(ty', trexp else') then {exp=(), ty=ty'}
          else (print("Error: Invalid if conditional statement at pos " ^ Integer.toString(pos)); {exp=(), ty=Types.BOTTOM})
        end
    (*TODO how to think about comparison ops? Just that they must be the same type and map to int?*)
    |   trexp(A.WhileExp{test, body} = if checkInt(trexp test) andalso checkSameType({exp=(), Types.UNIT}, trexp body) then {exp=(), Types.UNIT}
                                       else (print("Error: While loop construction error at " ^ Integer.toString(pos));{exp=(), ty=Types.BOTTOM})
    |   trexp(A.ForExp{var, lo, hi, body, pos}) = if checkInt(trexp lo, pos) andalso checkInt(trexp hi, pos) andalso checkSameType({exp=(), Types.UNIT}, trexp body) then {exp=(), Types.UNIT}
                                                  else (print("Error: For loop construction error at " ^ Integer.toString(pos)); {exp=(), ty=Types.BOTTOM})
  in
    trexp(root)
  end

end
