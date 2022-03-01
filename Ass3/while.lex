 (* pi.lex *)
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
    ("new", T.NEW)
    ])
end;

open KeyWord;
