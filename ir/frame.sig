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
	val makestring2 : string Temp.Table.table -> Temp.temp -> string
	val tempMap: register Temp.Table.table
    val SP : Temp.temp
    val FP : Temp.temp
    val ra : Temp.temp

    val argregs: Temp.temp list
    val temps : Temp.temp list
    val calleeSaves : Temp.temp list
    val callerSaves : Temp.temp list
    val returnRegs : Temp.temp list

    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
	val procEntryExit3 : frame * Assem.instr list -> {prolog:string, body: Assem.instr list, epilog: string}

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
	val string : frag -> string
    val wordSize : int
    val K: int

    val registerColors : unit -> register list 

end
