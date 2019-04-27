structure Assem = struct

  type reg = string
  type temp = Temp.temp
  type label = Temp.label

  datatype instr =
                OPER of {assem: string,
    			    dst: temp list,
    			    src: temp list,
    			    jump: label list option}
            |   LABEL of {assem: string,
                    lab: Temp.label}
            |   MOVE of {assem: string,
    			    dst: temp,
    			    src: temp}
	
  fun check ([], []) = true
  |   check ([], a) = false
  |   check (a, []) = false
  |   check (a1::l1, a2::l2) = if a1=a2 then check(l1,l2) else false
  
  fun checkj (NONE, NONE) = true 
  |   checkj (NONE, a) = false 
  |   checkj (a, NONE) = false 
  |   checkj (j1, j2) = 
		let 
			val j1' = map (Symbol.name) (valOf(j1))
			val j2' = map (Symbol.name) (valOf(j2))
		in
			check(j1',j2')
		end
  
  fun equal ((MOVE{assem=a1, dst=d1, src=s1}), (MOVE{assem=a2, dst=d2, src=s2})) =d1=d2 andalso s1=s2
  |   equal ((LABEL{assem=a1, lab=l1}), (LABEL{assem=a2, lab=l2})) = (Symbol.name l1) = (Symbol.name l2)
  |   equal ((OPER{assem=a1, dst=d1, src=s1, jump=j1}), (OPER{assem=a2, dst=d2, src=s2, jump=j2})) = a1=a2 andalso check(d1,d2) andalso check(s1,s2) andalso checkj(j1,j2)
  |   equal (_, _) = false

  fun format saytemp =
    let
	fun speak(assem,dst,src,jump) =
	    let val saylab = Symbol.name
		fun f(#"`":: #"s":: i::rest) =
		    (#"$"::explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
		  | f( #"`":: #"d":: i:: rest) =
		    (#"$"::explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
		  | f( #"`":: #"j":: i:: rest) =
		    (explode(saylab(List.nth(jump,ord i - ord #"0"))) @ f rest)
		  | f( #"`":: #"`":: rest) = #"`" :: f rest
		  | f( #"`":: _ :: rest) = ErrorMsg.impossible "bad Assem format"
		  | f(c :: rest) = (c :: f rest)
		  | f nil = nil
	    in implode(f(explode assem))
	    end
      in fn OPER{assem,dst,src,jump=NONE} => let val s = speak(assem,dst,src,nil) in if String.size s > 0 then "\t" ^ s else s end
          | OPER{assem,dst,src,jump=SOME j} => let val s = speak(assem,dst,src,j) in if String.size s > 0 then "\t" ^ s else s end
	  | LABEL{assem,...} => assem
	  | MOVE{assem,dst,src} => let val s = speak(assem,[dst],[src],nil) in if String.size s > 0 then "\t" ^ s else s end
     end

end
