functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "tiger.grm"*)
(*#line 12.1 "tiger.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\148\000\005\000\148\000\007\000\148\000\009\000\148\000\
\\011\000\148\000\013\000\148\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\148\000\027\000\148\000\
\\031\000\148\000\032\000\148\000\035\000\148\000\036\000\148\000\
\\038\000\148\000\039\000\148\000\043\000\148\000\044\000\148\000\
\\045\000\148\000\000\000\
\\001\000\001\000\149\000\005\000\149\000\007\000\149\000\009\000\149\000\
\\011\000\149\000\013\000\149\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\149\000\027\000\149\000\
\\031\000\149\000\032\000\149\000\035\000\149\000\036\000\149\000\
\\038\000\149\000\039\000\149\000\043\000\149\000\044\000\149\000\
\\045\000\149\000\000\000\
\\001\000\001\000\150\000\005\000\150\000\007\000\150\000\009\000\150\000\
\\011\000\150\000\013\000\150\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\150\000\027\000\150\000\
\\031\000\150\000\032\000\150\000\035\000\150\000\036\000\150\000\
\\038\000\150\000\039\000\150\000\043\000\150\000\044\000\150\000\
\\045\000\150\000\000\000\
\\001\000\001\000\151\000\005\000\151\000\007\000\151\000\009\000\151\000\
\\011\000\151\000\013\000\151\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\151\000\027\000\151\000\
\\031\000\151\000\032\000\151\000\035\000\151\000\036\000\151\000\
\\038\000\151\000\039\000\151\000\043\000\151\000\044\000\151\000\
\\045\000\151\000\000\000\
\\001\000\001\000\152\000\005\000\152\000\007\000\152\000\009\000\152\000\
\\011\000\152\000\013\000\152\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\152\000\027\000\152\000\
\\031\000\152\000\032\000\152\000\035\000\152\000\036\000\152\000\
\\038\000\152\000\039\000\152\000\043\000\152\000\044\000\152\000\
\\045\000\152\000\000\000\
\\001\000\001\000\153\000\005\000\153\000\007\000\153\000\009\000\153\000\
\\011\000\153\000\013\000\153\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\153\000\027\000\153\000\
\\031\000\153\000\032\000\153\000\035\000\153\000\036\000\153\000\
\\038\000\153\000\039\000\153\000\043\000\153\000\044\000\153\000\
\\045\000\153\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\001\000\002\000\039\000\000\000\
\\001\000\002\000\049\000\000\000\
\\001\000\002\000\065\000\000\000\
\\001\000\002\000\066\000\000\000\
\\001\000\002\000\067\000\000\000\
\\001\000\002\000\074\000\000\000\
\\001\000\002\000\098\000\012\000\097\000\029\000\096\000\000\000\
\\001\000\002\000\100\000\000\000\
\\001\000\002\000\103\000\000\000\
\\001\000\002\000\103\000\013\000\112\000\000\000\
\\001\000\002\000\106\000\000\000\
\\001\000\002\000\121\000\000\000\
\\001\000\002\000\127\000\000\000\
\\001\000\002\000\131\000\000\000\
\\001\000\005\000\170\000\009\000\170\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\019\000\026\000\020\000\025\000\
\\021\000\024\000\022\000\023\000\023\000\022\000\024\000\021\000\
\\026\000\020\000\027\000\019\000\000\000\
\\001\000\005\000\171\000\009\000\171\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\019\000\026\000\020\000\025\000\
\\021\000\024\000\022\000\023\000\023\000\022\000\024\000\021\000\
\\026\000\020\000\027\000\019\000\000\000\
\\001\000\005\000\172\000\013\000\172\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\019\000\026\000\020\000\025\000\
\\021\000\024\000\022\000\023\000\023\000\022\000\024\000\021\000\
\\026\000\020\000\027\000\019\000\000\000\
\\001\000\005\000\173\000\013\000\173\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\019\000\026\000\020\000\025\000\
\\021\000\024\000\022\000\023\000\023\000\022\000\024\000\021\000\
\\026\000\020\000\027\000\019\000\000\000\
\\001\000\005\000\089\000\013\000\088\000\000\000\
\\001\000\005\000\093\000\009\000\092\000\000\000\
\\001\000\005\000\115\000\009\000\114\000\000\000\
\\001\000\005\000\115\000\013\000\122\000\000\000\
\\001\000\006\000\082\000\028\000\081\000\000\000\
\\001\000\006\000\116\000\000\000\
\\001\000\006\000\125\000\019\000\124\000\000\000\
\\001\000\007\000\072\000\009\000\071\000\000\000\
\\001\000\007\000\072\000\039\000\094\000\000\000\
\\001\000\008\000\083\000\000\000\
\\001\000\011\000\078\000\015\000\030\000\016\000\029\000\017\000\028\000\
\\018\000\027\000\019\000\026\000\020\000\025\000\021\000\024\000\
\\022\000\023\000\023\000\022\000\024\000\021\000\026\000\020\000\
\\027\000\019\000\000\000\
\\001\000\011\000\091\000\015\000\030\000\016\000\029\000\017\000\028\000\
\\018\000\027\000\019\000\026\000\020\000\025\000\021\000\024\000\
\\022\000\023\000\023\000\022\000\024\000\021\000\026\000\020\000\
\\027\000\019\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\
\\031\000\070\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\
\\035\000\104\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\
\\036\000\069\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\
\\036\000\128\000\000\000\
\\001\000\019\000\080\000\000\000\
\\001\000\019\000\090\000\000\000\
\\001\000\019\000\119\000\000\000\
\\001\000\019\000\133\000\000\000\
\\001\000\028\000\068\000\000\000\
\\001\000\028\000\113\000\000\000\
\\001\000\038\000\064\000\043\000\038\000\044\000\037\000\045\000\036\000\000\000\
\\001\000\040\000\108\000\000\000\
\\001\000\040\000\110\000\000\000\
\\001\000\043\000\038\000\044\000\037\000\045\000\036\000\000\000\
\\136\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\010\000\018\000\014\000\017\000\028\000\016\000\000\000\
\\141\000\000\000\
\\142\000\017\000\028\000\018\000\027\000\000\000\
\\143\000\017\000\028\000\018\000\027\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\000\000\
\\147\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\000\000\
\\154\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\159\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\
\\032\000\105\000\000\000\
\\160\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\161\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\162\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\008\000\047\000\010\000\046\000\012\000\045\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\169\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\180\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\181\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\182\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\"
val actionRowNumbers =
"\007\000\056\000\052\000\055\000\
\\073\000\051\000\008\000\007\000\
\\007\000\007\000\007\000\053\000\
\\054\000\075\000\007\000\009\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\084\000\083\000\082\000\
\\080\000\048\000\010\000\011\000\
\\012\000\046\000\040\000\038\000\
\\057\000\033\000\078\000\013\000\
\\007\000\007\000\064\000\076\000\
\\036\000\063\000\062\000\006\000\
\\005\000\004\000\003\000\002\000\
\\001\000\061\000\060\000\059\000\
\\058\000\081\000\007\000\042\000\
\\030\000\035\000\007\000\007\000\
\\007\000\066\000\007\000\026\000\
\\043\000\037\000\027\000\022\000\
\\077\000\034\000\014\000\007\000\
\\015\000\016\000\039\000\071\000\
\\069\000\079\000\067\000\018\000\
\\007\000\049\000\065\000\007\000\
\\074\000\089\000\050\000\017\000\
\\090\000\085\000\047\000\028\000\
\\094\000\031\000\007\000\007\000\
\\044\000\024\000\007\000\023\000\
\\019\000\029\000\092\000\007\000\
\\032\000\016\000\020\000\041\000\
\\070\000\007\000\068\000\093\000\
\\091\000\086\000\007\000\021\000\
\\095\000\096\000\007\000\025\000\
\\087\000\045\000\072\000\007\000\
\\088\000\000\000"
val gotoT =
"\
\\001\000\002\000\002\000\133\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\033\000\008\000\032\000\009\000\031\000\010\000\030\000\
\\011\000\029\000\000\000\
\\000\000\
\\001\000\038\000\003\000\001\000\000\000\
\\001\000\039\000\003\000\001\000\000\000\
\\001\000\040\000\003\000\001\000\000\000\
\\001\000\042\000\003\000\001\000\004\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\046\000\003\000\001\000\000\000\
\\000\000\
\\001\000\048\000\003\000\001\000\000\000\
\\001\000\049\000\003\000\001\000\000\000\
\\001\000\050\000\003\000\001\000\000\000\
\\001\000\051\000\003\000\001\000\000\000\
\\001\000\052\000\003\000\001\000\000\000\
\\001\000\053\000\003\000\001\000\000\000\
\\001\000\054\000\003\000\001\000\000\000\
\\001\000\055\000\003\000\001\000\000\000\
\\001\000\056\000\003\000\001\000\000\000\
\\001\000\057\000\003\000\001\000\000\000\
\\001\000\058\000\003\000\001\000\000\000\
\\001\000\059\000\003\000\001\000\000\000\
\\001\000\060\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\061\000\009\000\031\000\010\000\030\000\011\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\071\000\000\000\
\\001\000\073\000\003\000\001\000\000\000\
\\001\000\075\000\003\000\001\000\005\000\074\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\042\000\003\000\001\000\004\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\082\000\003\000\001\000\000\000\
\\001\000\083\000\003\000\001\000\000\000\
\\001\000\084\000\003\000\001\000\000\000\
\\000\000\
\\001\000\085\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\093\000\000\000\
\\001\000\097\000\003\000\001\000\000\000\
\\000\000\
\\013\000\100\000\014\000\099\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\105\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\107\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\100\000\014\000\109\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\115\000\003\000\001\000\000\000\
\\001\000\116\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\118\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\121\000\003\000\001\000\000\000\
\\000\000\
\\013\000\124\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\127\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\128\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\130\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\132\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 134
val numrules = 59
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit | STRING of unit ->  (string) | INT of unit ->  (int) | ID of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 32) => true | (T 33) => true | (T 34) => true | (T 40) => true | (T 36) => true | (T 37) => true | (T 38) => true | (T 42) => true | (T 43) => true | (T 44) => true | (T 28) => true | (T 29) => true | (T 30) => true | (T 31) => true | (T 35) => true | (T 39) => true | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 31))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "UMINUS"
  | (T 25) => "AND"
  | (T 26) => "OR"
  | (T 27) => "ASSIGN"
  | (T 28) => "ARRAY"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "WHILE"
  | (T 33) => "FOR"
  | (T 34) => "TO"
  | (T 35) => "DO"
  | (T 36) => "LET"
  | (T 37) => "IN"
  | (T 38) => "END"
  | (T 39) => "OF"
  | (T 40) => "BREAK"
  | (T 41) => "NIL"
  | (T 42) => "FUNCTION"
  | (T 43) => "VAR"
  | (T 44) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 in ((*#line 41.19 "tiger.grm"*)(*#line 532.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  STRING1 = STRING1 ()
 in ((*#line 44.42 "tiger.grm"*)(*#line 538.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  INT1 = INT1 ()
 in ((*#line 45.41 "tiger.grm"*)(*#line 544.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 3, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 46.41 "tiger.grm"*)(*#line 550.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, lvalue1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 in ((*#line 47.41 "tiger.grm"*)(*#line 554.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 in ((*#line 48.41 "tiger.grm"*)(*#line 560.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, MINUS1left, exp1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 49.41 "tiger.grm"*)(*#line 566.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 50.41 "tiger.grm"*)(*#line 573.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 51.41 "tiger.grm"*)(*#line 580.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 52.41 "tiger.grm"*)(*#line 587.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 53.41 "tiger.grm"*)(*#line 594.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 54.41 "tiger.grm"*)(*#line 601.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 55.41 "tiger.grm"*)(*#line 608.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 56.41 "tiger.grm"*)(*#line 615.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 57.41 "tiger.grm"*)(*#line 622.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 58.41 "tiger.grm"*)(*#line 629.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 59.41 "tiger.grm"*)(*#line 636.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 60.41 "tiger.grm"*)(*#line 643.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 val  exp1 = exp1 ()
 in ((*#line 61.41 "tiger.grm"*)(*#line 650.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, lvalue1left, exp1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID explist1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  explist1 = explist1 ()
 in ((*#line 62.41 "tiger.grm"*)(*#line 657.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID expseq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  expseq1 = expseq1 ()
 in ((*#line 63.41 "tiger.grm"*)(*#line 664.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID field1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  field1 = field1 ()
 in ((*#line 64.41 "tiger.grm"*)(*#line 670.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 65.41 "tiger.grm"*)(*#line 677.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 66.41 "tiger.grm"*)(*#line 685.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, IF1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ((*#line 67.41 "tiger.grm"*)(*#line 692.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, IF1left, exp3right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 68.41 "tiger.grm"*)(*#line 700.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, WHILE1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ((*#line 69.41 "tiger.grm"*)(*#line 707.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, FOR1left, exp3right), rest671)
end
|  ( 27, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 70.41 "tiger.grm"*)(*#line 716.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 28, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID expseq1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID declarationlist1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  declarationlist1 = declarationlist1 ()
 val  expseq1 = expseq1 ()
 in ((*#line 71.40 "tiger.grm"*)(*#line 720.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ((*#line 73.33 "tiger.grm"*)(*#line 727.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 val  ID1 = ID1 ()
 in ((*#line 74.32 "tiger.grm"*)(*#line 733.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 2, ( result, lvalue1left, ID1right), rest671)
end
|  ( 31, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 val  exp1 = exp1 ()
 in ((*#line 75.32 "tiger.grm"*)(*#line 740.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 2, ( result, lvalue1left, RBRACK1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 in ((*#line 77.32 "tiger.grm"*)(*#line 747.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 3, ( result, exp1left, exp1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ntVOID expseq1, expseq1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  expseq1 = expseq1 ()
 val  exp1 = exp1 ()
 in ((*#line 78.31 "tiger.grm"*)(*#line 753.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 3, ( result, expseq1left, exp1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 in ((*#line 80.29 "tiger.grm"*)(*#line 760.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 4, ( result, exp1left, exp1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ntVOID explist1, explist1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  explist1 = explist1 ()
 val  exp1 = exp1 ()
 in ((*#line 81.27 "tiger.grm"*)(*#line 766.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 4, ( result, explist1left, exp1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ((*#line 83.31 "tiger.grm"*)(*#line 773.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 5, ( result, ID1left, exp1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID field1, field1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  field1 = field1 ()
 val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ((*#line 84.30 "tiger.grm"*)(*#line 780.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 5, ( result, field1left, exp1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ntVOID declaration1, declaration1left, declaration1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  declaration1 = declaration1 ()
 in ((*#line 86.40 "tiger.grm"*)(*#line 788.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 6, ( result, declaration1left, declaration1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ntVOID declaration1, _, declaration1right)) :: ( _, ( MlyValue.ntVOID declarationlist1, declarationlist1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  declarationlist1 = declarationlist1 ()
 val  declaration1 = declaration1 ()
 in ((*#line 87.36 "tiger.grm"*)(*#line 794.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 6, ( result, declarationlist1left, declaration1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.ntVOID typedeclaration1, typedeclaration1left, typedeclaration1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  typedeclaration1 = typedeclaration1 ()
 in ((*#line 89.31 "tiger.grm"*)(*#line 801.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 7, ( result, typedeclaration1left, typedeclaration1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ntVOID variabledeclaration1, variabledeclaration1left, variabledeclaration1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  variabledeclaration1 = variabledeclaration1 ()
 in ((*#line 90.28 "tiger.grm"*)(*#line 807.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 7, ( result, variabledeclaration1left, variabledeclaration1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.ntVOID functiondeclaration1, functiondeclaration1left, functiondeclaration1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  functiondeclaration1 = functiondeclaration1 ()
 in ((*#line 91.28 "tiger.grm"*)(*#line 813.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 7, ( result, functiondeclaration1left, functiondeclaration1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ((*#line 93.45 "tiger.grm"*)(*#line 819.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 9, ( result, VAR1left, exp1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ((*#line 94.36 "tiger.grm"*)(*#line 826.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 9, ( result, VAR1left, exp1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: _ :: ( _, ( MlyValue.ntVOID typefields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  typefields1 = typefields1 ()
 val  exp1 = exp1 ()
 in ((*#line 96.71 "tiger.grm"*)(*#line 834.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 10, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID typefields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  typefields1 = typefields1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ((*#line 97.62 "tiger.grm"*)(*#line 842.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 10, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 in ((*#line 100.29 "tiger.grm"*)(*#line 851.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 4, ( result, exp1left, exp1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ntVOID explist1, explist1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  explist1 = explist1 ()
 val  exp1 = exp1 ()
 in ((*#line 101.27 "tiger.grm"*)(*#line 857.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 4, ( result, explist1left, exp1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ((*#line 103.31 "tiger.grm"*)(*#line 864.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 5, ( result, ID1left, exp1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID field1, field1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  field1 = field1 ()
 val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ((*#line 104.30 "tiger.grm"*)(*#line 871.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 5, ( result, field1left, exp1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.ntVOID type1, _, type1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  type1 = type1 ()
 in ((*#line 106.35 "tiger.grm"*)(*#line 879.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 8, ( result, TYPE1left, type1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ((*#line 108.34 "tiger.grm"*)(*#line 886.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 53, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID typefields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  typefields1 = typefields1 ()
 in ((*#line 109.33 "tiger.grm"*)(*#line 892.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 54, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 110.34 "tiger.grm"*)(*#line 898.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 11, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ((*#line 111.34 "tiger.grm"*)(*#line 902.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.ntVOID typefield1, typefield1left, typefield1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  typefield1 = typefield1 ()
 in ((*#line 113.42 "tiger.grm"*)(*#line 908.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 13, ( result, typefield1left, typefield1right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.ntVOID typefield1, _, typefield1right)) :: _ :: ( _, ( MlyValue.ntVOID typefields1, typefields1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  typefields1 = typefields1 ()
 val  typefield1 = typefield1 ()
 in ((*#line 114.42 "tiger.grm"*)(*#line 914.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 13, ( result, typefields1left, typefield1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ((*#line 116.25 "tiger.grm"*)(*#line 921.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 12, ( result, ID1left, ID2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(ParserData.MlyValue.VOID,p1,p2))
end
end
