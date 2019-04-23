structure Types =
struct

  type unique = unit ref

  datatype ty =
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
      	  | NAME of Symbol.symbol * ty option ref
      	  | UNIT
          | BOTTOM

  fun ty2str (RECORD(_): ty) = "Record"
  |   ty2str NIL = "Nil"
  |   ty2str INT = "Int"
  |   ty2str STRING = "String"
  |   ty2str (ARRAY(_):ty) = "Array"
  |   ty2str (NAME(_):ty) = "Name"
  |   ty2str UNIT = "Unit"
  |   ty2str BOTTOM = "Bottom"

end
