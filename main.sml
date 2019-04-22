structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame
   (*structure R = RegAlloc*)
   

 fun getsome (SOME x) = x

   fun emitproc out (F.PROC{body,frame}) =
    let (*val _ = print ("\nemit " ^ F.name frame ^ "\n")*)
          (*val _ = Printtree.printtree(out,body); *)
	   val stms = Canon.linearize body
     (*val _ = app (fn s => Printtree.printtree(out,s)) stms;*)
     val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
     (*val _ = app (fn s => Printtree.printtree(out,s)) stms'*)
	   val instrs =   List.concat(map (Mipsgen.codegen frame) stms')
     val instrs' = F.procEntryExit2 (frame,instrs)
     
	   val (asl, allocation) = Regalloc.alloc (instrs', frame, true)
     val {prolog,body,epilog} = F.procEntryExit3(frame, asl)
     
     val format0 = Assem.format(F.makestring2 allocation)
     (*val format0 = Assem.format(F.makestring)*)
      in
		
        app (fn i => TextIO.output(out,format0 i)) body
		
     end
    | emitproc out (f as F.STRING(lab,s)) = TextIO.output(out,F.string(f))


   fun withOpenFile fname f =
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out)
	    handle e => (TextIO.closeOut out; raise e)
       end

   fun compile filename =
       let
	       val _ = Tr.reset()
		   val absyn = Parse.parse filename
           val tup = (FindEscape.findEscape absyn; Semant.transProg absyn)
           val frags = #1 tup
           val errors = #2 tup
           val name = String.substring(filename, 0, (String.size filename) - 4)
           fun f (x as F.PROC(_)) = true
           |   f (x as F.STRING(_)) = false
           val (func, str) = List.partition f frags
           fun stdlib2str () = 
            let
              val stdLibStr = TextIO.inputAll(TextIO.openIn("./stdlib/stdlib.s"))
            in
              "\n" ^ stdLibStr
            end
           fun g out = (
              TextIO.output(out, ".globl tig_main\n");
              TextIO.output(out, ".data\n");
              app (emitproc out) str;
              TextIO.output(out, "\n.text\n");
              app (emitproc out) func;
              TextIO.output (out, stdlib2str())
            )
        in
            if errors then ()
            else (
                withOpenFile (name ^ ".s") g
            )
       end

end
