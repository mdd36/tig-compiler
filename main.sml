structure Go =
struct
fun go file =
  let
    val _ = ErrorMsg.reset()
  in
    Semant.transProg(Parse.parse file)
  end
end
