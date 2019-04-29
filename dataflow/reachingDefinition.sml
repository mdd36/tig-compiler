signature REACHINGDEFINITION=
sig
	val RDanalysis :
			Flow.flowgraph * Flow.Graph.node list * (Temp.temp -> Flow.Graph.node list)->
				(Flow.Graph.node -> Flow.Graph.node list) * (Flow.Graph.node -> Flow.Graph.node list)
	val eliminatedeadcode: 
			Assem.instr list * (string -> Flow.Graph.node) * Flow.flowgraph * (Flow.Graph.node -> Temp.temp list) ->
				Assem.instr list
	val propagation:
			Assem.instr list * (string -> Flow.Graph.node) * Flow.flowgraph * (Flow.Graph.node -> Flow.Graph.node list) ->
				Assem.instr list				

end

structure Reachingdefinition :> REACHINGDEFINITION =
struct
	structure NodeSet = RedBlackSetFn(type ord_key=Flow.Graph.node val compare=Flow.Graph.compare)
	structure NodeMap = RedBlackMapFn(type ord_key=Flow.Graph.node val compare=Flow.Graph.compare) (*node -> livein set * liveout set*)
	structure AssemMap = RedBlackMapFn(type ord_key=string val compare=String.compare)
	structure A = Assem
	exception DivBy0 of string
	fun RDanalysis (fg as Flow.FGRAPH{control = cfg,def = deft, use = uset, ismove = imt}, nodes, getdefs) =
		let
			fun getGenKill () =
				let
					fun oneNode (node, (g, k)) =
						case Flow.Graph.Table.look(deft, node) of NONE => (NodeMap.insert(g, node, NodeSet.empty), NodeMap.insert(k, node, NodeSet.empty))
															| SOME tl => if tl = [] then (NodeMap.insert(g, node, NodeSet.empty), NodeMap.insert(k, node, NodeSet.empty))
																		else (
																			let
																				val kill = foldl (fn (n, se) => NodeSet.union(se, NodeSet.addList(NodeSet.empty, getdefs n))) NodeSet.empty tl
																			in																	
																				(NodeMap.insert(g, node, NodeSet.singleton(node)), NodeMap.insert(k, node, NodeSet.difference(kill,NodeSet.singleton(node))))
																			end)
						
				in
					foldl oneNode (NodeMap.empty, NodeMap.empty) nodes
				end
			val (gen, kill) = getGenKill()
			
			fun oneTime (in', out') =
				let
					fun check (node, (b,inmap,outmap)) =
						let 
							val i' = valOf(NodeMap.find(inmap, node))
							val o' = valOf(NodeMap.find(outmap, node))
							val pres = Flow.Graph.pred node
							val i'' = foldl (fn(n, se) => NodeSet.union(se, valOf(NodeMap.find(outmap, n)))) NodeSet.empty pres
							val o'' = NodeSet.union(valOf(NodeMap.find(gen, node)), NodeSet.difference(i'', valOf(NodeMap.find(kill, node))))
						in 						
							(b andalso NodeSet.equal(i', i'') andalso NodeSet.equal(o', o''), NodeMap.insert(inmap, node, i''), NodeMap.insert(outmap, node, o''))
						end
				in
					foldl check (true, in', out') nodes
				end
						
			
			fun iteration (rdin, rdout) = 
				let 
					val (b, inmap, outmap) = oneTime(rdin, rdout)
				in
					if b then (inmap, outmap) else iteration(inmap, outmap)
				end
			
			
			val (rdin, rdout) = iteration(foldl (fn (n, nmap) => NodeMap.insert(nmap, n, NodeSet.empty)) NodeMap.empty nodes, foldl (fn (n, nmap) => NodeMap.insert(nmap, n, NodeSet.empty)) NodeMap.empty nodes)
			
			fun getRDin node = case NodeMap.find(rdin, node) of NONE => (print("reachingDefinition.sml: ERROR: No such node found in RDin \n"); [])
															| SOME nl => (NodeSet.listItems nl)
			fun getRDout node = case NodeMap.find(rdout, node) of NONE => (print("reachingDefinition.sml: ERROR: No such node found in RDout \n"); [])
															| SOME nl => (NodeSet.listItems nl)
			
		in
			(getRDin, getRDout)
		end
	
	fun getname (a, n) = a^(Int.toString n)
	
	fun getnumber s num = valOf(Int.fromString(String.substring(s, num, (size s) -1-num)))
	fun removeSquiggle x = if x < 0 then "-" ^ Int.toString (~x) else Int.toString x
	
	fun contains([], _) = false
	|	contains(a::l, x) = (a=x orelse contains(l, x))
	
	fun contains2([], _) = false
	|	contains2((a,b)::l, x) = (a=x orelse contains2(l, x))
	
	fun contains3([], x) = x
	|	contains3((a,b)::l, x) = if a=x then b else contains3(l, x)
	
	
	
	fun eliminatedeadcode (assemlist, assem2node, fg as Flow.FGRAPH{control = cfg,def = deft, use = uset, ismove = imt}, getlo) =
		let
			val j = ref 0
			fun check (a as (A.OPER{assem, dst, src, jump}), al) = 
				let
					val _ = (j:= (!j)+1)
					val node = assem2node (getname(assem,!j))
					val outs = getlo node
					val b = foldl (fn (d, bo) => bo andalso ((not (contains(outs, d))) andalso (not (isSome(Temp.Table.look(MipsFrame.tempMap, d))))) ) true dst
				in
					if dst <> [] andalso b then al else a::al
				end
			|   check (a as (A.MOVE{assem, dst, src}), al) = 
				let
					val _ = (j:= (!j)+1)
					val node = assem2node (getname(assem,!j))
					val outs = getlo node
					val b = contains(outs, dst) orelse isSome(Temp.Table.look(MipsFrame.tempMap, dst))
				in
					if not b then al else a::al
				end
			|   check (a as (A.LABEL{assem, lab}), al) = (j:=(!j)+1 ; a :: al)
		in
			rev (foldl check [] assemlist)
		end
		
	
	fun propagation (assemlist, assem2node, fg as Flow.FGRAPH{control = cfg,def = deft, use = uset, ismove = imt}, getRDin) =
		let
			val j = ref 0
			fun getConst () =
				let 
					val i = ref 0
					fun cins (A.OPER{assem, dst, src, jump}, (nm,mm)) =
							(i:=(!i)+1;
							if String.isSubstring "li" assem then (NodeMap.insert(nm, assem2node (getname(assem,!i)), getnumber assem 8),mm)
							else (nm,mm))
					  | cins (A.MOVE{assem, dst, src}, (nm,mm)) =
							if dst = src then (nm,mm)
							else 
							(i:=(!i)+1;
							(nm, NodeMap.insert(mm, assem2node (getname(assem,!i)), src)))
					  | cins (a, (nm,mm)) = (i:=(!i)+1;(nm,mm) )
				in
					foldl cins (NodeMap.empty, NodeMap.empty) assemlist
				end
			
			val (nmap, mmap) = getConst()
			val (nmap, mmap) = (ref nmap, ref mmap)
			fun count (n::ns, s) = (case Flow.Graph.Table.look(deft, n) of NONE => count(ns,s)
															| SOME d => if contains(d,s) then n::count(ns,s) else count(ns,s))
			|   count ([], s) = []
			
			fun searchC (n, s)= 
				let 
					val innodes = getRDin n 
					val ns = count(innodes,s)
				in	
					if length(ns)=1 then NodeMap.find(!nmap, hd ns)
					else NONE
				end
			
			fun searchT (n, s)= 
				let 
					val innodes = getRDin n 
					val ns = count(innodes,s)
				in	
					if length(ns)=1 then NodeMap.find(!mmap, hd ns)
					else NONE
				end
			
					
			
			fun check (a as (A.MOVE{assem, dst, src}), al) = 
				if (src=dst) then al
				else (
					if isSome(Temp.Table.look(MipsFrame.tempMap, dst)) then ((j:=(!j)+1);a::al)
					else(
					let 
						val _ = (j:=(!j)+1)
						val node = assem2node (getname(assem,!j))
						val c = searchC(node,src)
					in
						if isSome c then (nmap:= NodeMap.insert(!nmap, node,  valOf c);
									mmap:= #1 (NodeMap.remove(!mmap, node));
									(A.OPER{assem="li `d0, " ^ removeSquiggle (valOf(c)) ^ "\n", src=[], dst=[dst], jump=NONE})::al)
						else (a::al)
					end))
					
			|   check (a as (A.LABEL{assem, lab}), al) = (j:=(!j)+1 ; a :: al)
			|   check (a as (A.OPER{assem, dst, src, jump}), al) = 
				let 
					val _ = (j:=(!j)+1)
					val node = assem2node (getname(assem,!j))
					fun getmatch (s, (cl,tl)) = 
						case searchC(node, s) of NONE => (case searchT(node, s) of NONE => (cl,tl)
																				| SOME t => (cl, (s, t)::tl))
											| SOME c => ((s,c)::cl,tl)
											
					val (cl,tml) = foldl getmatch ([],[]) src
					val cl = rev cl
					val tml = rev tml
					fun modifyall (s::sl) = if contains2(cl,s) then sl 
										else contains3(tml,s)::modifyall(sl)	
					|   modifyall [] = []
					
					fun modifyt (s::sl) = contains3(tml,s)::modifyt(sl)	
					|   modifyt [] = []
											
				in
					if cl=[] then ((A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
					else (
						case String.substring(assem,0,4) of 
							(*"sw '" => (nmap:= NodeMap.insert(!nmap, node, #2 (hd cl));
									  (A.OPER{assem="li `d0, " ^ removeSquiggle (#2 (hd cl)) ^ "\n", src=[], dst=dst, jump=jump})::al)*)
							"sw `" => (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al 
							
						|	"addi" => (let val num = (#2 (hd cl)) + (getnumber assem 15)
									   in 
										(nmap:= NodeMap.insert(!nmap, node, num);
									    (A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
									   end)
						|	"andi" => (let val num = Word.toInt(Word.andb(Word.fromInt(#2 (hd cl)), Word.fromInt(getnumber assem 15)))
									   in
									   (nmap:= NodeMap.insert(!nmap, node, num);
									   (A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
									   end)
						|	"ori " => (let val num = Word.toInt(Word.orb(Word.fromInt(#2 (hd cl)), Word.fromInt(getnumber assem 14)))
									   in
									   (nmap:= NodeMap.insert(!nmap, node, num);
									  (A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
									  end)
						|	"xori" => (let val num = Word.toInt(Word.xorb(Word.fromInt(#2 (hd cl)), Word.fromInt(getnumber assem 15)))
									   in
									   (nmap:= NodeMap.insert(!nmap, node, num);
									  (A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
									  end)
						|	"sll " => (let val num = Word.toInt(Word.<<(Word.fromInt(#2 (hd cl)), Word.fromInt(getnumber assem 14)))
									   in
									   (nmap:= NodeMap.insert(!nmap, node, num);
									   (A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
									   end)
						|	"sra " => (let val num = Word.toInt(Word.~>>(Word.fromInt(#2 (hd cl)), Word.fromInt(getnumber assem 14)))
									   in
									   (nmap:= NodeMap.insert(!nmap, node, num);
									   (A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
									   end)
						|	"srl " => (let val num = Word.toInt(Word.>>(Word.fromInt(#2 (hd cl)), Word.fromInt(getnumber assem 14)))
									   in
									   (nmap:= NodeMap.insert(!nmap, node, num);
									   (A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
									   end)
							
						|	"add " => (if length cl = 2
									   then (let val num = (#2 (hd cl)) + (#2 (hd (tl cl)))
											 in 
												(nmap:= NodeMap.insert(!nmap, node, num);
												(A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
											 end)
									   else (A.OPER{assem="addi `d0, `s0, " ^ removeSquiggle (#2 (hd cl)) ^ "\n", src=modifyall src, dst=dst, jump=jump})::al)
						|	"sub " => (if length cl = 2
									   then (let val num = (#2 (hd cl)) - (#2 (hd (tl cl)))
											 in 
												(nmap:= NodeMap.insert(!nmap, node, num);
												(A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
											 end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"mul " => (if length cl = 2
									   then (let val num = (#2 (hd cl)) * (#2 (hd (tl cl)))
											 in 
												(nmap:= NodeMap.insert(!nmap, node, num);
												(A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
											 end)
									   else (if (#2 (hd cl))=0
											then (A.OPER{assem="li `d0, " ^ Int.toString 0^ "\n", src=[], dst=dst, jump=jump})::al
											else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al))
						|	"div " => (if length cl = 2
									   then (let val num1 = #2 (hd cl)
												 val num2 = #2 (hd (tl cl))
											 in 
												(if num2 = 0 then raise DivBy0 "Divide by zero found"
												else 
												(nmap:= NodeMap.insert(!nmap, node, num1 div num2);
												(A.OPER{assem="li `d0, " ^ removeSquiggle (num1 div num2)^ "\n", src=[], dst=dst, jump=jump})::al))
											 end)
									   else (if (#1 (hd cl)) = (hd (tl src)) andalso (#2 (hd cl))=0 then raise DivBy0 "Divide by zero found"
											 else (if (#1 (hd cl)) = (hd src) andalso (#2 (hd cl))=0 
													then (A.OPER{assem="li `d0, " ^ Int.toString 0^ "\n", src=[], dst=dst, jump=jump})::al
													else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)))
		   
						|	"and " => (if length cl = 2
									   then (let val num = Word.toInt(Word.andb(Word.fromInt(#2 (hd cl)), Word.fromInt((#2 (hd (tl cl)))))) 
											 in 
												(nmap:= NodeMap.insert(!nmap, node, num);
												(A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
											 end)
									   else (A.OPER{assem="andi `d0, `s0, " ^ removeSquiggle (#2 (hd cl)) ^ "\n", src=modifyall src, dst=dst, jump=jump})::al)
						|	"or  " => (if length cl = 2
									   then (let val num = Word.toInt(Word.orb(Word.fromInt(#2 (hd cl)), Word.fromInt(#2 (hd (tl cl))))) 
											 in 
												(nmap:= NodeMap.insert(!nmap, node, num);
												(A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
											 end)
									   else (A.OPER{assem="ori `d0, `s0, " ^ removeSquiggle (#2 (hd cl)) ^ "\n", src=modifyall src, dst=dst, jump=jump})::al)
						|	"xor " => (if length cl = 2
									   then (let val num = Word.toInt(Word.xorb(Word.fromInt(#2 (hd cl)), Word.fromInt(#2 (hd (tl cl))))) 
											 in 
												(nmap:= NodeMap.insert(!nmap, node, num);
												(A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
											 end)
									   else (A.OPER{assem="xori `d0, `s0, " ^ removeSquiggle (#2 (hd cl)) ^ "\n", src=modifyall src, dst=dst, jump=jump})::al)
						|	"sllv" => (if length cl = 2
									   then (let val num = Word.toInt(Word.<<(Word.fromInt(#2 (hd cl)), Word.fromInt(#2 (hd (tl cl)))))  
											 in 
												(nmap:= NodeMap.insert(!nmap, node, num);
												(A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
											 end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"srlv" => (if length cl = 2
									   then (let val num = Word.toInt(Word.>>(Word.fromInt(#2 (hd cl)), Word.fromInt(#2 (hd (tl cl)))))
											 in 
												(nmap:= NodeMap.insert(!nmap, node, num);
												(A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
											 end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"srav" => (if length cl = 2
									   then (let val num = Word.toInt(Word.~>>(Word.fromInt(#2 (hd cl)), Word.fromInt(#2 (hd (tl cl)))))  
											 in 
												(nmap:= NodeMap.insert(!nmap, node, num);
												(A.OPER{assem="li `d0, " ^ removeSquiggle (num)^ "\n", src=[], dst=dst, jump=jump})::al)
											 end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)		   
										
						|	"bltz" => (let 
									      val b = (#2 (hd cl)) <0
										  val t = hd (valOf(jump))
									   in
										  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
									   end)
						|	"blez" => (let 
									      val b = (#2 (hd cl)) <=0
										  val t = hd (valOf(jump))
									   in
										  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
									   end)
						|	"bgtz" => (let 
									      val b = (#2 (hd cl)) >0
										  val t = hd (valOf(jump))
									   in
										  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
									   end)
						|	"bgez" => (let 
									      val b = (#2 (hd cl)) >=0
										  val t = hd (valOf(jump))
									   in
										  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
									   end)
						|	"beqz" => (let 
									      val b = (#2 (hd cl)) =0
										  val t = hd (valOf(jump))
									   in
										  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
									   end)									   
						|	"bnez" => (let 
									      val b = (#2 (hd cl)) <>0
										  val t = hd (valOf(jump))
									   in
										  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
									   end)
						|	"blt " => (if length cl = 2 
										then (
										   let 
											  val b1 = (#2 (hd cl))
											  val b2 = (#2 (hd (tl cl)))
											  val t = hd (valOf(jump))
											  val b = b1 < b2
										   in
											  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
										   end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"ble " => (if length cl = 2 
										then (
										   let 
											  val b1 = (#2 (hd cl))
											  val b2 = (#2 (hd (tl cl)))
											  val t = hd (valOf(jump))
											  val b = b1 <= b2
										   in
											  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
										   end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"bltu" => (if length cl = 2 
										then (
										   let 
											  val b1 = (#2 (hd cl))
											  val b2 = (#2 (hd (tl cl)))
											  val t = hd (valOf(jump))
											  val b = b1 < b2
										   in
											  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
										   end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"bleu" => (if length cl = 2 
										then (
										   let 
											  val b1 = (#2 (hd cl))
											  val b2 = (#2 (hd (tl cl)))
											  val t = hd (valOf(jump))
											  val b = b1 <= b2
										   in
											  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
										   end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"bgt " => (if length cl = 2 
										then (
										   let 
											  val b1 = (#2 (hd cl))
											  val b2 = (#2 (hd (tl cl)))
											  val t = hd (valOf(jump))
											  val b = b1 > b2
										   in
											  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
										   end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"bge " => (if length cl = 2 
										then (
										   let 
											  val b1 = (#2 (hd cl))
											  val b2 = (#2 (hd (tl cl)))
											  val t = hd (valOf(jump))
											  val b = b1 >= b2
										   in
											  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
										   end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"bgtu" => (if length cl = 2 
										then (
										   let 
											  val b1 = (#2 (hd cl))
											  val b2 = (#2 (hd (tl cl)))
											  val t = hd (valOf(jump))
											  val b = b1 > b2
										   in
											  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
										   end)
									   else a::al)
						|	"bgeu" => (if length cl = 2 
										then (
										   let 
											  val b1 = (#2 (hd cl))
											  val b2 = (#2 (hd (tl cl)))
											  val t = hd (valOf(jump))
											  val b = b1 >= b2
										   in
											  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
										   end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"beq " => (if length cl = 2 
										then (
										   let 
											  val b1 = (#2 (hd cl))
											  val b2 = (#2 (hd (tl cl)))
											  val t = hd (valOf(jump))
											  val b = b1 = b2
										   in
											  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
										   end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|	"bne " => (if length cl = 2 
										then (
										   let 
											  val b1 = (#2 (hd cl))
											  val b2 = (#2 (hd (tl cl)))
											  val t = hd (valOf(jump))
											  val b = b1 <> b2
										   in
											  if b then (A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [t]})::al else al
										   end)
									   else (A.OPER{assem=assem, src=modifyt src, dst=dst, jump=jump})::al)
						|   _      => (a::al) 			   		
						)
					
				end
				
				
		in
			rev (foldl check [] assemlist)
		end

end
