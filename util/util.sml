structure Util =
struct

	datatype IntOrStr = 
		INT of int 
		| STR of string

	fun println STR x = print(x ^ "\n")
	|	println INT x = println (if x < 0 then println ("-" ^ Int.toString (~x)) else println (Int.toString x))

	fun printf f x = print(f x)

	fun printlnf f x = println (f x)
end