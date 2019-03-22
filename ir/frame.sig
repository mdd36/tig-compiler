signature FRAME =
sig
    type frame
    type access

    val newFrame : {name: Temp.label, formals: bool list} -> frame
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
    val externalCall : string * Tree.exp list -> Tree.exp
    val find : access -> Tree.exp -> Tree.exp

    val SP : Temp.temp
    val FP : Temp.temp

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

    val wordSize : int

end