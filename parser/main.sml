structure Go =
struct
fun go file =
  let
    val _ = ErrorMsg.reset()
  in
    Semant.transExp(Semant.venv, Semant.tenv, Parse.parse file)
  end
end
