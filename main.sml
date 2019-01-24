structure Go =
struct
fun go file =
  let
    val _ = ErrorMsg.reset()
  in
    Parse.parse file
  end
end
