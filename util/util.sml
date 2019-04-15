structure Util =
struct

	datatype IntOrStr = 
		INT of int 
		| STR of string

	fun println STR x = print(x ^ "\n")
	|	println INT x = println (if x < 0 then println ("-" ^ Int.toString (~x)) else println (Int.toString x))

	fun printf f x = print(f x)

	fun printlnf f x = println (f x)

	fun contains([], _) = false
	|	contains(a::l, x) = (a=x orelse contains(l, x))

	fun replace old new = fn sample => if old = sample then new else sample 

	
end