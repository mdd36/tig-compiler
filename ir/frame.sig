signature FRAME =
sig
    type frame
    type access
	type register
    val newFrame : {name: Temp.label, formals: bool list} -> frame
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
    val externalCall : string * Tree.exp list -> Tree.exp
    val find : access -> Tree.exp -> Tree.exp
    val name : frame -> string
	val makestring : Temp.temp -> string
	val tempMap: register Temp.Table.table
    val SP : Temp.temp
    val FP : Temp.temp
    val ra : Temp.temp

    val argregs: Temp.temp list
    val temps : Temp.temp list
    val calleeSaves : Temp.temp list
    val callerSaves : Temp.temp list
    val returnRegs : Temp.temp list

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
	val string : frag -> string
    val wordSize : int

end
