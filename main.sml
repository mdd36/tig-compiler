structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame
   (*structure R = RegAlloc*)

 fun getsome (SOME x) = x

   fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ F.name frame ^ "\n")
         (* val _ = Printtree.printtree(out,body);*)
	 val stms = Canon.linearize body
         (* val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	 val instrs =   List.concat(map (Mipsgen.codegen frame) stms')
     
         val format0 = Assem.format(F.makestring)
      in  app (fn i => TextIO.output(out,format0 i)) instrs
     end
    | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(F.STRING(lab,s)))

   fun withOpenFile fname f =
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out)
	    handle e => (TextIO.closeOut out; raise e)
       end

   fun compile filename =
       let
	       val _ = Tr.reset()
		   val absyn = Parse.parse filename
           val tup = ((*FindEscape.prog absyn; *)Semant.transProg absyn)
           val frags = #1 tup
           val errors = #2 tup
        in
            if errors then ()
            else (
                withOpenFile (filename ^ ".s")
    	     (fn out => (app (emitproc out) frags))
            )
       end

end
