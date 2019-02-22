structure A = Absyn

structure Semant :
  sig
    type expty
    (*val transVar: (venv * tenv * A.var) -> expty*)
    val transExp: venv * tenv * A.exp -> expty
    (*val transDec: venv * tenv * A.dec -> {venv: venv, tenv: tenv}
    val transTy :        tenv * A.ty  -> Types.ty*)
  end =
struct
  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}



  fun assert(exp, msg) = if exp then () else raise msg

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

  fun transExp(venv, tenv, root) =
  let
  (*|   trexp(SOME x) = trexp(x) Not sure about this*)
  fun   trexp(A.IntExp i) = {exp=(), ty=Types.INT}
    |   trexp(A.StringExp (s,pos)) = {exp=(), ty=Types.STRING}
    |   trexp(A.NilExp) = {exp=(), ty=Types.UNIT}
    |   trexp(A.OpExp{left, oper=A.PlusOp, right, pos}) =
          if checkInt(trexp(left), pos) andalso checkInt(trexp(right), pos) then {exp=(), ty=Types.INT}
          else {exp=(), ty=Types.BOTTOM}
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
          if checkInt(trexp test, pos) andalso checkSameType(expty',  trexp getOpt(else', A.NilExp)) then {exp=(), ty=(#ty expty')}
          else (print("Error: Invalid if conditional statement at pos " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
        end
    (*TODO how to think about comparison ops? Just that they must be the same type and map to int?*)
    |   trexp(A.WhileExp{test, body, pos}) = if checkInt(trexp test, pos) andalso checkSameType({exp=(), ty=Types.UNIT}, trexp body) then {exp=(), ty=Types.UNIT}
                                       else (print("Error: While loop construction error at " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
    |   trexp(A.ForExp{var, lo, hi, body, escape, pos}) = if checkInt(trexp lo, pos) andalso checkInt(trexp hi, pos) andalso checkSameType({exp=(), ty=Types.UNIT}, trexp body) then {exp=(), ty=Types.UNIT}
                                                  else (print("Error: For loop construction error at " ^ Int.toString(pos)); {exp=(), ty=Types.BOTTOM})
  in
    trexp(root)
  end

end
