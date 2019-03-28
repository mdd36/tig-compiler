signature CODEGEN = 
sig 
	structure Frame : FRAME
	val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure Codegen: CODEGEN = struct
	structure Frame : FRAME = MipsFrame
	structure T = Tree
	structure A= Assem
  
	fun codegen (frame) (stm: Tree.stm) : Assem.instr list = 
		let 
			val ilist = ref (nil: A.instr list)
			fun emit x = ilist := x :: !ilist
			fun result (gen) = let val t = Temp.newtemp() in gen t; t end
			
			fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
								
			  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)),e2)) =
					emit(A.OPER{assem = "sw 's1, " ^int i ^ "('s0)\n",
								src = [munchExp e1, munchExp e2],
								dst = [],
								jump = NONE})
								
			  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)),e2)) =
					emit(A.OPER{assem = "sw 's1, " ^int i ^ "('s0)\n",
								src = [munchExp e1, munchExp e2],
								dst = [],
								jump = NONE})
								
			  | munchStm (T.MOVE(T.MEM(e1), T.MEM(e2))) =
					emit(A.OPER{assem = "move 's0, 's1\n",
								src = [munchExp e1, munchExp e2],
								dst = [],
								jump = NONE})
								
			  | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) =
					emit(A.OPER{assem = "sw 's0, " ^int i ^ "($0)\n",
								src = [munchExp e2],
								dst = [],
								jump = NONE})
								
			  | munchStm (T.MOVE(T.MEM(e1), e2)) =
					emit(A.OPER{assem = "sw 's1, " ^int 0 ^ "('s0)\n",
								src = [munchExp e1, munchExp e2],
								dst = [],
								jump = NONE})
								
			  | munchStm (T.MOVE(T.TEMP i, e2)) =
					emit(A.OPER{assem = "sw 's1, " ^int 0 ^ "('s0)\n",
								src = [munchExp e2],
								dst = [i],
								jump = NONE})
								
			  | munchStm (T.JUMP(e, labs)) =
					emit(A.OPER{assem = "j 's0\n",
								src = [munchExp e],
								dst = [],
								jump = labs})
								 
			  | munchStm (T.LABEL lab) =
					emit(A.LABEL{assem = lab ^ ":\n",
								 lab = lab})
				
			  | munchStm (T.EXP e) = (munchExp e; ())
		
		in	
			munchStm stm;
			rev(!ilist)
		end
			
			
	
end
