structure MipsFrame : FRAME =
struct
    datatype access = InReg of Temp.temp | InFrame of int
    type frame = {name: Temp.label, formals: bool list, locals: int ref}

    
    fun newFrame {name, formals} = {name=name, formals=formals, locals=ref 0}
    fun formals {name, formals, locals} =
        let
            fun findAccess (formal, offset) = if formal then InFrame((offset+1)*(~4)) (*Going up the stack by machine word increments*)
                                           else InReg(Temp.newtemp())
            fun buildOffsets([], depth) = []
            |   buildOffsets(a::l, depth) = if a then (depth+1)::buildOffsets(l, depth+1) else depth::buildOffsets(l, depth)
            val offsets = buildOffsets(formals, 0)
        in
            ListPair.foldr (fn(formal, offset, res) => findAccess(formal, offset)::res) [] (formals, offsets)
        end
    fun allocLocal {name, formals, locals} =
        fn bool' => (
            let
                fun findAccess escapes offset = if escapes then (!offset = !offset + 1; InFrame((!offset+1)*(~4)))
                                                else InReg(Temp.newtemp())
            in
                !locals = !locals + 1;
                findAccess bool' locals
            end
        )
end
