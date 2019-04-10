structure Ab = Absyn

structure FindEscape:
sig
    val findEscape : Ab.exp -> unit
end =
struct
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table

    fun traverseVar (env, depth', sym) =
        let
            fun travVar (Ab.SimpleVar(id, pos)) =
                (case Symbol.look(env, id) of
                    SOME(dep, esc) => if depth' > dep then esc := true else ()
                |   NONE => ()
                )
            |   travVar (Ab.SubscriptVar(var, exp, pos)) =
                    (travVar(var); traverseExp(env, depth', exp))
            |   travVar(Ab.FieldVar(var, name, pos)) = travVar(var)
        in
            travVar sym
        end
    and traverseExp (env, depth', sym) =
        let
            fun travExp(Ab.StringExp _) = ()
            |   travExp(Ab.NilExp) = ()
            |   travExp(Ab.IntExp _) = ()
            |   travExp(Ab.SeqExp(l)) = app (fn (exp, _) => travExp exp) l
			|   travExp(Ab.OpExp{left, oper, right, pos}) = (travExp left; travExp right)
            |   travExp(Ab.AssignExp{var, exp, pos}) = (traverseVar(env, depth', var); travExp(exp))
            |   travExp(Ab.IfExp{test, then', else'=SOME(exp), pos}) = (travExp(test); travExp(then'); travExp(exp))
            |   travExp(Ab.IfExp{test, then', else'=NONE, pos}) = (travExp(test); travExp(then'))
            |   travExp(Ab.WhileExp{test, body, pos}) = (travExp(test); travExp(body))
            |   travExp(Ab.ForExp{var, escape, lo, hi, body, pos}) =
                    let
                        val env' = Symbol.enter(env, var, (depth', escape))
                    in
                        traverseExp(env', depth', body);
                        travExp(lo);
                        travExp(hi)
                    end
            |   travExp(Ab.BreakExp _) = ()
            |   travExp(Ab.CallExp{func, args, pos}) = app travExp args
            |   travExp(Ab.ArrayExp{typ, size, init, pos}) = (travExp(size); travExp(init))
            |   travExp(Ab.VarExp var) = traverseVar(env, depth', var)
            |   travExp(Ab.RecordExp{fields,typ,pos}) = app (fn(s,e,p) => travExp e) fields
            |   travExp(Ab.LetExp{decs,body,pos}) = traverseExp(traverseDecs(env,depth',decs), depth', body)
        in
            travExp sym
        end

    and traverseDecs (env, depth', sym) =
        let
            fun travDec(Ab.FunctionDec(l), env) =
                let
                    fun f ({name, params, result, body, pos}) =
                        let
                            fun g ({name, escape, typ, pos}, env) =
                                Symbol.enter(env, name, (depth'+1, escape))
                            val env' = foldl g env params
                        in
                            traverseExp(env', depth'+1, body)
                        end
                in
                    app f l;
                    env
                end
            |   travDec(Ab.TypeDec(l), env) = env
            |   travDec(Ab.VarDec{name, typ, init, escape, pos}, env) =
                    Symbol.enter(env, name, (depth', escape))
        in
            foldl travDec env sym
        end
    fun findEscape(prog) = traverseExp(Symbol.empty, 0, prog)
end
