type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
val s=ref ""
val comment_nest = ref 0
val last_open_comment = ref ~1
val string_in = ref 0
val last_open_string = ref ~1
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let
  val pos = hd(!linePos)
  fun check_open_comments () = if !comment_nest <> 0 then
                                ErrorMsg.error (!last_open_comment) ("Comment never closed")
                               else ()
  fun check_open_string () = if !string_in <> 0 then
                                ErrorMsg.error (!last_open_string) ("unclosed string")
                               else ()
in
  check_open_comments();
  comment_nest := 0;
  last_open_comment := ~1;
  check_open_string();
  string_in := 0;
  last_open_comment := ~1;
  Tokens.EOF(pos,pos)
end

%%
%s COMMENT STR FMT;
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%%

<INITIAL> (" "|\t)+ => (continue());
<INITIAL> "/*" => (YYBEGIN COMMENT;
                    comment_nest := !comment_nest + 1;
                    last_open_comment := yypos;
                    continue());
<COMMENT> "/*" => (comment_nest := !comment_nest + 1;
                   continue());
<COMMENT> "*/" => (if !comment_nest = 1 then YYBEGIN INITIAL
                 else ();
                 comment_nest := !comment_nest - 1;
                 continue());
<COMMENT> . => ( continue());
<INITIAL> \" => (YYBEGIN STR; string_in := 1; last_open_string := yypos; s:=""; continue());
<STR> \" => ( YYBEGIN INITIAL; string_in := 0; Tokens.STRING(!s, !last_open_string, yypos+1));

<STR> \\\\ => (s:=(!s)^"\\";   continue());
<STR> \\\" => (s:=(!s)^"\"";   continue());
<STR> \\n => (s:=(!s)^"\n";   continue());
<STR> \\t => (s:=(!s)^"\t";   continue());
<STR> \\a => (s:=(!s)^"\a";   continue());
<STR> \\b => (s:=(!s)^"\b";   continue());
<STR> \\r => (s:=(!s)^"\r";   continue());
<STR> \\v => (s:=(!s)^"\v";   continue());

<STR> \\[0-1][0-9][0-9]|\\2[0-4][0-9]|\\25[0-5] =>
  (s := !s ^ String.str(chr(valOf(Int.fromString(String.substring(yytext,1,3))))); continue());

<STR> \\\^[@-_] => (s := !s ^
						String.str(chr(ord(String.sub(yytext,2))-64)); continue());

<STR> \\[\ \n\r\t\f] => (YYBEGIN FMT;
                    if String.substring(yytext,1,1) = "\n" orelse String.substring(yytext,1,1) = "\r" then
                      (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue())
                    else continue());
<FMT> [\ \t\f] => (continue());
<FMT> [\n\r] => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<FMT> \\ => (YYBEGIN STR; continue());
<FMT> . => (ErrorMsg.error yypos ("Illegal token in formatting block: " ^ yytext); continue());

<STR> \\ => (ErrorMsg.error yypos "Illegal escape character"; continue());
<STR> [ -~] => (s := !s ^ yytext; continue());
<STR> [\n\r] => (s := !s ^ yytext;lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<STR> . => (ErrorMsg.error yypos ("Illegal string character, likely a bad escape: " ^ yytext); continue());


[\n\r] => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());


<INITIAL>type  => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>function  => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>break  => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>of  => (Tokens.OF(yypos,yypos+2));
<INITIAL>end  => (Tokens.END(yypos,yypos+3));
<INITIAL>in  => (Tokens.IN(yypos,yypos+2));
<INITIAL>nil  => (Tokens.NIL(yypos,yypos+3));
<INITIAL>let  => (Tokens.LET(yypos,yypos+3));
<INITIAL>do  => (Tokens.DO(yypos,yypos+2));
<INITIAL>to  => (Tokens.TO(yypos,yypos+2));
<INITIAL>for  => (Tokens.FOR(yypos,yypos+3));
<INITIAL>while  => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>else  => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>then  => (Tokens.THEN(yypos,yypos+4));
<INITIAL>if  => (Tokens.IF(yypos,yypos+2));
<INITIAL>array  => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>":="  => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>"|"  => (Tokens.OR(yypos,yypos+1));
<INITIAL>"&"  => (Tokens.AND(yypos,yypos+1));
<INITIAL>">="  => (Tokens.GE(yypos,yypos+2));
<INITIAL>">"  => (Tokens.GT(yypos,yypos+1));
<INITIAL>"<="  => (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"  => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<>"  => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"="  => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"/"  => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"*"  => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"-"  => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"+"  => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"."  => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"}"  => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"{"  => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"]"  => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"["  => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>")"  => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"("  => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>";"  => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"  => (Tokens.COLON(yypos,yypos+1));
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL> [a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID(yytext,yypos,yypos+size yytext));
<INITIAL> [0-9]+ => (Tokens.INT(valOf(Int.fromString yytext),yypos,yypos+size yytext));
<INITIAL> .   => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
