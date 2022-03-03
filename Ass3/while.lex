 (* while.lex *)
structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val badCh : string * string * int * int -> unit = fn
    (fileName,bad,line,col) =>
    TextIO.output(TextIO.stdOut,fileName^"["
        ^Int.toString line^"."^Int.toString col
        ^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin,!col);

 (* Keyword management *)
structure KeyWord :
sig val find : string ->
            (int * int -> (svalue,int) token) option
end =
struct
    val TableSize = 422 (* 211 *)
    val HashFactor = 5
    val hash = fn
        s => List.foldr (fn (c,v) =>
        (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
val HashTable = Array.array(TableSize,nil) :
            (string * (int * int -> (svalue,int) token))
            list Array.array
val add = fn
    (s,v) => let val i = hash s
        in Array.update(HashTable,i,(s,v)
            :: (Array.sub(HashTable, i)))
        end
val find = fn
    s => let val i = hash s
            fun f ((key,v)::r) = if s=key then SOME v
                                else f r
                | f nil = NONE
        in f (Array.sub(HashTable, i))
        end
val _ = (List.app add [
    ("read",T.READ),
    ("write",T.WRITE),
    ("if",T.IF),
    ("then",T.THEN),
    ("else",T.ELSE),
    ("endif",T.ENDIF),
    ("while",T.WHILE),
    ("do",T.DO),
    ("endwh",T.ENDWH),
    ("int",T.INT),
    ("bool",T.BOOL),
    ("var",T.VAR),
    ("program",T.PROGRAM),
    ("tt",T.TT),
    ("ff",T.FF)
    ])
end;

open KeyWord;

%%
%full
%header (functor WhileLexFun(structure Tokens: While_TOKENS));
%arg (fileName:string);
%s WHILE COMMENT;
alpha = [A-Za-z];
hexa = "0"("x"|"X")[0-9A-Fa-f];
digit = [0-9];
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");

%%
<INITIAL>{ws}* => (lin:=1; eolpos:=0;
                YYBEGIN WHILE; continue ());
<WHILE>{ws}* => (continue ());
<WHILE>{eol} => (lin:=(!lin)+1;
            eolpos:=yypos+size yytext; continue ());
<WHILE>{alpha}+ => (case find yytext of
                    SOME v => (col:=yypos-(!eolpos);
                                v(!lin,!col))
                    | _ => (col:=yypos-(!eolpos);
                            T.ID(yytext,!lin,!col)));
<WHILE>{digit}+ => (T.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !lin, !col));
<WHILE>"::" => (col:=yypos-(!eolpos); T.START(!lin,!col));
<WHILE>":" => (col:=yypos-(!eolpos); T.TYPEOF(!lin,!col));
<WHILE>";" => (col:=yypos-(!eolpos); T.EOS(!lin,!col));
<WHILE>"," => (col:=yypos-(!eolpos); T.COMMA(!lin,!col));
<WHILE>"{" => (col:=yypos-(!eolpos); T.LSPAR(!lin,!col));
<WHILE>"}" => (col:=yypos-(!eolpos); T.RSPAR(!lin,!col));
<WHILE>":=" => (col:=yypos-(!eolpos); T.ASSIGN(!lin,!col));
<WHILE>"~" => (col:=yypos-(!eolpos); T.NEGATE(!lin,!col));
<WHILE>"(" => (col:=yypos-(!eolpos); T.LPAR(!lin,!col));
<WHILE>")" => (col:=yypos-(!eolpos); T.RPAR(!lin,!col));
<WHILE>"!" => (col:=yypos-(!eolpos); T.NOT(!lin,!col));
<WHILE>"&&" => (col:=yypos-(!eolpos); T.AND(!lin,!col));
<WHILE>"<" => (col:=yypos-(!eolpos); T.LT(!lin,!col));
<WHILE>"<=" => (col:=yypos-(!eolpos); T.LEQ(!lin,!col));
<WHILE>"=" => (col:=yypos-(!eolpos); T.EQ(!lin,!col));
<WHILE>">" => (col:=yypos-(!eolpos); T.GT(!lin,!col));
<WHILE>">=" => (col:=yypos-(!eolpos); T.GEQ(!lin,!col));
<WHILE>"<>" => (col:=yypos-(!eolpos); T.NEQ(!lin,!col));
<WHILE>"+" => (col:=yypos-(!eolpos); T.PLUS(!lin,!col));
<WHILE>"-" => (col:=yypos-(!eolpos); T.MINUS(!lin,!col));
<WHILE>"*" => (col:=yypos-(!eolpos); T.TIMES(!lin,!col));
<WHILE>"/" => (col:=yypos-(!eolpos); T.DIV(!lin,!col));
<WHILE>"%" => (col:=yypos-(!eolpos); T.MOD(!lin,!col));
<WHILE>"||" => (col:=yypos-(!eolpos); T.OR(!lin,!col));
<WHILE>. => (col:=yypos-(!eolpos);
        badCh (fileName,yytext,!lin,!col);
        T.ILLCH(!lin,!col));
