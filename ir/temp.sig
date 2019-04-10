signature TEMP = 
sig
  eqtype temp
  val newtemp : unit -> temp
  structure Table : TABLE sharing type Table.key = temp
  val makestring: temp -> string
  type label = Symbol.symbol
  val newlabel : unit -> label
  val namedlabel : string -> label
  val empty : 'a Table.table
  val enter : 'a Table.table * temp * 'a -> 'a Table.table
  val look  : 'a Table.table * temp -> 'a option
end

