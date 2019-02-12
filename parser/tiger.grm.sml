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
(*#line 1.2 "tiger.grm"*)structure A = Absyn;

(*#line 13.1 "tiger.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\168\000\005\000\168\000\007\000\168\000\009\000\168\000\
\\011\000\168\000\013\000\168\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\168\000\027\000\168\000\
\\031\000\168\000\032\000\168\000\035\000\168\000\036\000\168\000\
\\038\000\168\000\039\000\168\000\043\000\168\000\044\000\168\000\
\\045\000\168\000\000\000\
\\001\000\001\000\169\000\005\000\169\000\007\000\169\000\009\000\169\000\
\\011\000\169\000\013\000\169\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\169\000\027\000\169\000\
\\031\000\169\000\032\000\169\000\035\000\169\000\036\000\169\000\
\\038\000\169\000\039\000\169\000\043\000\169\000\044\000\169\000\
\\045\000\169\000\000\000\
\\001\000\001\000\170\000\005\000\170\000\007\000\170\000\009\000\170\000\
\\011\000\170\000\013\000\170\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\170\000\027\000\170\000\
\\031\000\170\000\032\000\170\000\035\000\170\000\036\000\170\000\
\\038\000\170\000\039\000\170\000\043\000\170\000\044\000\170\000\
\\045\000\170\000\000\000\
\\001\000\001\000\171\000\005\000\171\000\007\000\171\000\009\000\171\000\
\\011\000\171\000\013\000\171\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\171\000\027\000\171\000\
\\031\000\171\000\032\000\171\000\035\000\171\000\036\000\171\000\
\\038\000\171\000\039\000\171\000\043\000\171\000\044\000\171\000\
\\045\000\171\000\000\000\
\\001\000\001\000\172\000\005\000\172\000\007\000\172\000\009\000\172\000\
\\011\000\172\000\013\000\172\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\172\000\027\000\172\000\
\\031\000\172\000\032\000\172\000\035\000\172\000\036\000\172\000\
\\038\000\172\000\039\000\172\000\043\000\172\000\044\000\172\000\
\\045\000\172\000\000\000\
\\001\000\001\000\173\000\005\000\173\000\007\000\173\000\009\000\173\000\
\\011\000\173\000\013\000\173\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\026\000\173\000\027\000\173\000\
\\031\000\173\000\032\000\173\000\035\000\173\000\036\000\173\000\
\\038\000\173\000\039\000\173\000\043\000\173\000\044\000\173\000\
\\045\000\173\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\009\000\086\000\016\000\011\000\030\000\010\000\033\000\009\000\
\\034\000\008\000\037\000\007\000\041\000\006\000\042\000\005\000\
\\046\000\085\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\046\000\047\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\046\000\089\000\000\000\
\\001\000\002\000\041\000\000\000\
\\001\000\002\000\052\000\000\000\
\\001\000\002\000\070\000\000\000\
\\001\000\002\000\071\000\000\000\
\\001\000\002\000\072\000\000\000\
\\001\000\002\000\081\000\000\000\
\\001\000\002\000\110\000\012\000\109\000\029\000\108\000\000\000\
\\001\000\002\000\112\000\000\000\
\\001\000\002\000\116\000\000\000\
\\001\000\002\000\116\000\009\000\115\000\000\000\
\\001\000\002\000\116\000\013\000\126\000\000\000\
\\001\000\002\000\119\000\000\000\
\\001\000\002\000\137\000\000\000\
\\001\000\002\000\144\000\000\000\
\\001\000\002\000\145\000\000\000\
\\001\000\002\000\149\000\000\000\
\\001\000\005\000\100\000\013\000\099\000\000\000\
\\001\000\005\000\104\000\009\000\103\000\000\000\
\\001\000\005\000\105\000\000\000\
\\001\000\005\000\129\000\009\000\128\000\000\000\
\\001\000\005\000\129\000\013\000\138\000\000\000\
\\001\000\006\000\092\000\028\000\091\000\000\000\
\\001\000\006\000\131\000\019\000\130\000\000\000\
\\001\000\006\000\132\000\000\000\
\\001\000\006\000\141\000\019\000\140\000\000\000\
\\001\000\007\000\077\000\009\000\076\000\000\000\
\\001\000\007\000\077\000\039\000\106\000\000\000\
\\001\000\007\000\079\000\000\000\
\\001\000\007\000\079\000\009\000\078\000\000\000\
\\001\000\008\000\093\000\000\000\
\\001\000\011\000\087\000\015\000\030\000\016\000\029\000\017\000\028\000\
\\018\000\027\000\019\000\026\000\020\000\025\000\021\000\024\000\
\\022\000\023\000\023\000\022\000\024\000\021\000\026\000\020\000\
\\027\000\019\000\000\000\
\\001\000\011\000\102\000\015\000\030\000\016\000\029\000\017\000\028\000\
\\018\000\027\000\019\000\026\000\020\000\025\000\021\000\024\000\
\\022\000\023\000\023\000\022\000\024\000\021\000\026\000\020\000\
\\027\000\019\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\
\\031\000\075\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\
\\035\000\117\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\
\\036\000\074\000\000\000\
\\001\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\
\\036\000\146\000\000\000\
\\001\000\019\000\090\000\000\000\
\\001\000\019\000\101\000\000\000\
\\001\000\019\000\135\000\000\000\
\\001\000\019\000\150\000\000\000\
\\001\000\019\000\152\000\000\000\
\\001\000\028\000\073\000\000\000\
\\001\000\028\000\127\000\000\000\
\\001\000\038\000\069\000\043\000\040\000\044\000\039\000\045\000\038\000\000\000\
\\001\000\040\000\121\000\000\000\
\\001\000\040\000\124\000\000\000\
\\001\000\043\000\040\000\044\000\039\000\045\000\038\000\000\000\
\\156\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\010\000\018\000\014\000\017\000\028\000\016\000\000\000\
\\161\000\000\000\
\\162\000\017\000\028\000\018\000\027\000\000\000\
\\163\000\017\000\028\000\018\000\027\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\000\000\
\\167\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\000\000\
\\174\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\180\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\
\\032\000\118\000\000\000\
\\181\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\182\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\183\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\008\000\050\000\010\000\049\000\012\000\048\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\191\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\192\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\193\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\194\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\195\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\045\000\038\000\000\000\
\\199\000\000\000\
\\200\000\043\000\040\000\000\000\
\\201\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\202\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\203\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\204\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\205\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\206\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\210\000\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\
\\019\000\026\000\020\000\025\000\021\000\024\000\022\000\023\000\
\\023\000\022\000\024\000\021\000\026\000\020\000\027\000\019\000\000\000\
\\211\000\000\000\
\\212\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\000\000\
\\216\000\000\000\
\\217\000\000\000\
\\218\000\000\000\
\\219\000\000\000\
\\220\000\000\000\
\"
val actionRowNumbers =
"\008\000\062\000\058\000\061\000\
\\080\000\057\000\011\000\008\000\
\\008\000\008\000\009\000\059\000\
\\060\000\083\000\008\000\012\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\095\000\096\000\094\000\
\\103\000\108\000\092\000\054\000\
\\013\000\014\000\015\000\052\000\
\\045\000\043\000\063\000\036\000\
\\086\000\039\000\016\000\008\000\
\\007\000\070\000\084\000\041\000\
\\069\000\068\000\006\000\005\000\
\\004\000\003\000\002\000\001\000\
\\067\000\066\000\065\000\064\000\
\\104\000\109\000\093\000\010\000\
\\047\000\032\000\040\000\008\000\
\\008\000\008\000\073\000\008\000\
\\082\000\008\000\027\000\048\000\
\\042\000\028\000\089\000\029\000\
\\072\000\085\000\037\000\038\000\
\\017\000\008\000\018\000\020\000\
\\044\000\078\000\076\000\087\000\
\\088\000\074\000\022\000\008\000\
\\055\000\071\000\008\000\008\000\
\\081\000\107\000\056\000\021\000\
\\110\000\097\000\053\000\030\000\
\\114\000\033\000\034\000\008\000\
\\008\000\049\000\105\000\008\000\
\\090\000\091\000\023\000\031\000\
\\112\000\008\000\035\000\019\000\
\\008\000\024\000\025\000\046\000\
\\077\000\008\000\075\000\113\000\
\\111\000\098\000\008\000\026\000\
\\115\000\100\000\050\000\116\000\
\\008\000\106\000\099\000\051\000\
\\008\000\079\000\008\000\102\000\
\\101\000\000\000"
val gotoT =
"\
\\001\000\002\000\002\000\153\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\035\000\008\000\034\000\009\000\033\000\010\000\032\000\
\\014\000\031\000\015\000\030\000\016\000\029\000\000\000\
\\000\000\
\\001\000\040\000\003\000\001\000\000\000\
\\001\000\041\000\003\000\001\000\000\000\
\\001\000\042\000\003\000\001\000\000\000\
\\001\000\044\000\003\000\001\000\004\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\049\000\003\000\001\000\000\000\
\\000\000\
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
\\001\000\061\000\003\000\001\000\000\000\
\\001\000\062\000\003\000\001\000\000\000\
\\001\000\063\000\003\000\001\000\000\000\
\\000\000\
\\010\000\064\000\000\000\
\\009\000\065\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\066\000\009\000\033\000\010\000\032\000\014\000\031\000\
\\015\000\030\000\016\000\029\000\000\000\
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
\\006\000\078\000\000\000\
\\001\000\080\000\003\000\001\000\000\000\
\\001\000\082\000\003\000\001\000\005\000\081\000\000\000\
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
\\000\000\
\\000\000\
\\001\000\044\000\003\000\001\000\004\000\086\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\092\000\003\000\001\000\000\000\
\\001\000\093\000\003\000\001\000\000\000\
\\001\000\094\000\003\000\001\000\000\000\
\\000\000\
\\001\000\095\000\003\000\001\000\000\000\
\\000\000\
\\001\000\096\000\003\000\001\000\000\000\
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
\\017\000\105\000\000\000\
\\001\000\109\000\003\000\001\000\000\000\
\\000\000\
\\012\000\112\000\013\000\111\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\118\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\120\000\003\000\001\000\000\000\
\\001\000\121\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\112\000\013\000\123\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\131\000\003\000\001\000\000\000\
\\001\000\132\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\134\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\137\000\003\000\001\000\000\000\
\\000\000\
\\012\000\140\000\000\000\
\\001\000\141\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\145\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\146\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\149\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\151\000\003\000\001\000\000\000\
\\000\000\
\\001\000\152\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 154
val numrules = 65
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
(nil
 $$ (T 18),nil
 $$ (T 27))::
(nil
 $$ (T 27),nil
 $$ (T 18))::
(nil
 $$ (T 31) $$ (T 6),nil
 $$ (T 31))::
(nil
,nil
 $$ (T 38) $$ (T 2) $$ (T 37))::
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
  | (T 45) => "ERROR"
  | (T 46) => "LOWPRIORITY"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 55.39 "tiger.grm"*)exp(*#line 604.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left), STRING1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (STRING as STRING1) = STRING1 ()
 in ((*#line 59.42 "tiger.grm"*)A.StringExp(STRING,STRINGleft)(*#line 610.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (INT as INT1) = INT1 ()
 in ((*#line 60.41 "tiger.grm"*)A.IntExp(INT)(*#line 616.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 3, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 61.41 "tiger.grm"*)A.NilExp(*#line 622.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, lvalue1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 in ((*#line 62.41 "tiger.grm"*)(*#line 626.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, (MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 63.41 "tiger.grm"*)A.OpExp{left = 0   , oper = A.MinusOp,  right = exp,                 pos = MINUSleft}(*#line 632.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, MINUS1left, exp1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, PLUSleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 64.41 "tiger.grm"*)A.OpExp{left = exp1, oper = A.PlusOp,   right = exp2,                pos = PLUSleft}(*#line 638.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, MINUSleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 65.41 "tiger.grm"*)A.OpExp{left = exp1, oper = A.MinusOp,  right = exp2,                pos = MINUSleft}(*#line 645.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, TIMESleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 66.41 "tiger.grm"*)A.OpExp{left = exp1, oper = A.TimesOp,  right = exp2,                pos = TIMESleft}(*#line 652.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, DIVIDEleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 67.41 "tiger.grm"*)A.OpExp{left = exp1, oper = A.DivideOp, right = exp2,                pos = DIVIDEleft}(*#line 659.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, ANDleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 68.41 "tiger.grm"*)A.IfExp{test = exp1, then' = exp2,      else' = SOME 0,              pos = ANDleft}(*#line 666.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, ORleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 69.41 "tiger.grm"*)A.IfExp{test = exp1, then' = 1,         else' = SOME exp2,           pos = ORleft}(*#line 673.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, EQleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 70.41 "tiger.grm"*)A.OpExp{left = exp1, oper = A.EqOp,     right = exp2,                pos = EQleft}(*#line 680.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, NEQleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 71.41 "tiger.grm"*)A.OpExp{left = exp1, oper = A.NeqOp,    right = exp2,                pos = NEQleft}(*#line 687.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, LTleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 72.41 "tiger.grm"*)A.OpExp{left = exp1, oper = A.LtOp,     right = exp2,                pos = LTleft}(*#line 694.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, LEleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 73.41 "tiger.grm"*)A.OpExp{left = exp1, oper = A.LeOp,     right = exp2,                pos = LEleft}(*#line 701.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, GTleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 74.41 "tiger.grm"*)A.OpExp{left = exp1, oper = A.GtOp,     right = exp2,                pos = GTleft}(*#line 708.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( _, GEleft, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 75.41 "tiger.grm"*)A.OpExp{left = exp1, oper = A.GeOp,     right = exp2,                pos = GEleft}(*#line 715.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, ASSIGNleft, _)) :: ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 76.41 "tiger.grm"*)A.AssignExp{var = lvalue, exp = exp,                                 pos = ASSIGNleft}(*#line 722.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, lvalue1left, exp1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID explist1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (explist as explist1) = explist1 ()
 in ((*#line 77.41 "tiger.grm"*)A.CallExp{func = Symbol.symbol ID,      args = explist,                   pos = IDleft}(*#line 729.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 in ((*#line 78.42 "tiger.grm"*)A.CallExp{func = Symbol.symbol ID,      args = [],                        pos = IDleft}(*#line 736.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID expseq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (expseq as expseq1) = expseq1 ()
 in ((*#line 79.41 "tiger.grm"*)A.SeqExp(expseq)(*#line 742.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID field1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (field as field1) = field1 ()
 in ((*#line 80.41 "tiger.grm"*)A.RecordExp{fields = field, typ = Symbol.symbol ID,                  pos=IDleft}(*#line 748.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 81.41 "tiger.grm"*)A.ArrayExp{typ = Symbol.symbol ID, size = exp1, init = exp2,              pos = IDleft}(*#line 755.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 82.41 "tiger.grm"*)A.IfExp{test = exp1, then' = exp2,      else' = NONE,                pos = IFleft}(*#line 763.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, IF1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ((*#line 83.41 "tiger.grm"*)A.IfExp{test = exp1, then' = exp2,      else' = SOME exp3,           pos = IFleft}(*#line 770.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, IF1left, exp3right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 84.41 "tiger.grm"*)A.WhileExp{test = exp1,  body = exp2,                                pos = WHILEleft}(*#line 778.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, WHILE1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ((*#line 85.41 "tiger.grm"*)A.ForExp{var = Symbol.symbol ID, escape = ref true, lo = exp1, hi = exp2, body = exp3, pos = FORleft}(*#line 785.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, FOR1left, exp3right), rest671)
end
|  ( 28, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 86.41 "tiger.grm"*)A.BreakExp(BREAKleft)(*#line 794.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 29, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID expseq1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID declarationlist1, _, _)) :: ( _, ( _, (LETleft as LET1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (declarationlist as declarationlist1) = declarationlist1 ()
 val  (expseq as expseq1) = expseq1 ()
 in ((*#line 87.41 "tiger.grm"*)A.LetExp{decs = declarationlist, body = expseq,                      pos = LETleft}(*#line 798.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 88.41 "tiger.grm"*)ERROR(*#line 805.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 in ((*#line 90.33 "tiger.grm"*)A.SimpleVar(Symbol.symbol(ID), IDleft)(*#line 809.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( MlyValue.ntVOID lvalue1, (lvalueleft as lvalue1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (lvalue as lvalue1) = lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in ((*#line 91.33 "tiger.grm"*)A.FieldVar(lvalue, Symbol.symbol(ID), lvalueleft) (*#line 815.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 2, ( result, lvalue1left, ID1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ntVOID lvalue1, (lvalueleft as lvalue1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 92.33 "tiger.grm"*)A.SubscriptVar(lvalue, exp, lvalueleft)(*#line 822.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 2, ( result, lvalue1left, RBRACK1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 95.32 "tiger.grm"*)exp :: [](*#line 829.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 3, ( result, exp1left, exp1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ntVOID expseq1, expseq1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (expseq as expseq1) = expseq1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 96.31 "tiger.grm"*)exp :: expseq(*#line 835.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 3, ( result, expseq1left, exp1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( _, ERROR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 97.30 "tiger.grm"*)exp :: ERROR(*#line 842.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 3, ( result, ERROR1left, exp1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 100.26 "tiger.grm"*)exp :: [](*#line 848.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 4, ( result, exp1left, exp1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ntVOID explist1, explist1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (explist as explist1) = explist1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 101.24 "tiger.grm"*)exp :: explist(*#line 854.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 4, ( result, explist1left, exp1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( _, ERROR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in ((*#line 102.24 "tiger.grm"*)exp :: ERROR(*#line 861.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 4, ( result, ERROR1left, exp1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.ntVOID declaration1, declaration1left, declaration1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (declaration as declaration1) = declaration1 ()
 in ((*#line 104.41 "tiger.grm"*)declaration :: [](*#line 867.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 6, ( result, declaration1left, declaration1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ntVOID declaration1, _, declaration1right)) :: ( _, ( MlyValue.ntVOID declarationlist1, declarationlist1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (declarationlist as declarationlist1) = declarationlist1 ()
 val  (declaration as declaration1) = declaration1 ()
 in ((*#line 105.37 "tiger.grm"*)declaration :: declarationlist(*#line 873.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 6, ( result, declarationlist1left, declaration1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.ntVOID tyDecs1, tyDecs1left, tyDecs1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (tyDecs as tyDecs1) = tyDecs1 ()
 in ((*#line 107.40 "tiger.grm"*)A.TypeDec(tyDecs)(*#line 880.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 7, ( result, tyDecs1left, tyDecs1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.ntVOID vardec1, vardec1left, vardec1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (vardec as vardec1) = vardec1 ()
 in ((*#line 108.20 "tiger.grm"*)vardec(*#line 886.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 7, ( result, vardec1left, vardec1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.ntVOID funDecs1, funDecs1left, funDecs1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (funDecs as funDecs1) = funDecs1 ()
 in ((*#line 109.37 "tiger.grm"*)A.FunctionDec(funDecs)(*#line 892.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 7, ( result, funDecs1left, funDecs1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 111.28 "tiger.grm"*)A.VarDec{name=Symbol.symbol(ID), escape=ref true,
                                        typ = Option.NONE, init=exp, pos=VARleft}(*#line 898.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 15, ( result, VAR1left, exp1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 113.37 "tiger.grm"*)A.VarDec{name=Symbol.symbol(ID1), escape=ref true,
                                        typ = Option.SOME((Symbol.symbol(ID2), ID2left)), init=exp, pos=VARleft}(*#line 906.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 15, ( result, VAR1left, exp1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: _ :: ( _, ( MlyValue.ntVOID typefields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (typefields as typefields1) = typefields1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 116.67 "tiger.grm"*)A.fundec{name = Symbol.symbol ID, params = typefields,
															result = NONE,
															body = exp,
															pos = FUNCTIONleft}(*#line 915.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 9, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 120.59 "tiger.grm"*)A.fundec{name = Symbol.symbol ID, params = [],
															result = NONE,
															body = exp,
															pos = FUNCTIONleft}(*#line 926.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 9, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID typefields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  (typefields as typefields1) = typefields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 124.78 "tiger.grm"*)A.fundec{name = Symbol.symbol ID1, params = typefields,
															result = SOME(Symbol.symbol ID2, ID2left),
															body = exp,
															pos = FUNCTIONleft}(*#line 936.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 9, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 128.67 "tiger.grm"*)A.fundec{name = Symbol.symbol ID1, params = [],
															result = SOME(Symbol.symbol ID2, ID2left),
															body = exp,
															pos = FUNCTIONleft}(*#line 948.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 9, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.ntVOID functiondeclaration1, functiondeclaration1left, functiondeclaration1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (functiondeclaration as functiondeclaration1) = functiondeclaration1 ()
 in ((*#line 133.31 "tiger.grm"*)functiondeclaration :: [](*#line 959.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 14, ( result, functiondeclaration1left, functiondeclaration1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ntVOID functiondeclaration1, _, functiondeclaration1right)) :: ( _, ( MlyValue.ntVOID funDecs1, funDecs1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (funDecs as funDecs1) = funDecs1 ()
 val  (functiondeclaration as functiondeclaration1) = functiondeclaration1 ()
 in ((*#line 134.39 "tiger.grm"*)functiondeclaration :: funDecs(*#line 965.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 14, ( result, funDecs1left, functiondeclaration1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 136.31 "tiger.grm"*)(Symbol.symbol(ID), exp, IDleft) :: [](*#line 972.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 5, ( result, ID1left, exp1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, IDleft, _)) :: _ :: ( _, ( MlyValue.ntVOID field1, field1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (field as field1) = field1 ()
 val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 137.30 "tiger.grm"*)(Symbol.symbol(ID), exp, IDleft) :: field(*#line 979.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 5, ( result, field1left, exp1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.ntVOID ty1, _, ty1right)) :: _ :: ( _, ( MlyValue.ID ID1, IDleft, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in ((*#line 139.33 "tiger.grm"*)A.TypeDec{name=Symbol.symbol(ID), ty=ty, pos=IDleft}(*#line 987.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 8, ( result, TYPE1left, ty1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.ntVOID typedeclaration1, typedeclaration1left, typedeclaration1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (typedeclaration as typedeclaration1) = typedeclaration1 ()
 in ((*#line 141.33 "tiger.grm"*)typedeclaration :: [](*#line 994.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 13, ( result, typedeclaration1left, typedeclaration1right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.ntVOID typedeclaration1, _, typedeclaration1right)) :: ( _, ( MlyValue.ntVOID tyDecs1, tyDecs1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (tyDecs as tyDecs1) = tyDecs1 ()
 val  (typedeclaration as typedeclaration1) = typedeclaration1 ()
 in ((*#line 142.33 "tiger.grm"*)typedeclaration :: tyDecs(*#line 1000.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 13, ( result, tyDecs1left, typedeclaration1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 in ((*#line 144.34 "tiger.grm"*)Symbol.symbol(ID)(*#line 1007.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 16, ( result, ID1left, ID1right), rest671)
end
|  ( 59, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID typefields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (typefields as typefields1) = typefields1 ()
 in ((*#line 145.34 "tiger.grm"*)A.RecordTy(typefields)(*#line 1013.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 16, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 60, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ((*#line 146.34 "tiger.grm"*)A.RecordTy([])(*#line 1019.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 16, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 in ((*#line 147.34 "tiger.grm"*)A.ArrayTy{name=Symbol.symbol(ID), pos=ARRAYleft}(*#line 1023.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 16, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.ntVOID typefield1, typefield1left, typefield1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (typefield as typefield1) = typefield1 ()
 in ((*#line 149.42 "tiger.grm"*)typefield :: [](*#line 1029.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 12, ( result, typefield1left, typefield1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.ntVOID typefield1, _, typefield1right)) :: _ :: ( _, ( MlyValue.ntVOID typefields1, typefields1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (typefields as typefields1) = typefields1 ()
 val  (typefield as typefield1) = typefield1 ()
 in ((*#line 150.42 "tiger.grm"*)typefields :: typefield(*#line 1035.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 12, ( result, typefields1left, typefield1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ((*#line 152.25 "tiger.grm"*)A.field{name = Symbol.symbol ID1, escape = ref true, typ = Symbol.symbol ID2, pos = ID1left}(*#line 1042.1 "tiger.grm.sml"*)
)
end; ()))
 in ( LrTable.NT 11, ( result, ID1left, ID2right), rest671)
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
fun ERROR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(ParserData.MlyValue.VOID,p1,p2))
fun LOWPRIORITY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(ParserData.MlyValue.VOID,p1,p2))
end
end
