functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* while.yacc *)
open DataTypes

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\000\000\
\\001\000\001\000\022\000\002\000\021\000\003\000\020\000\007\000\019\000\
\\021\000\018\000\000\000\
\\001\000\004\000\113\000\008\000\113\000\018\000\113\000\025\000\113\000\
\\027\000\113\000\028\000\113\000\029\000\113\000\030\000\113\000\
\\031\000\113\000\032\000\113\000\033\000\113\000\034\000\113\000\
\\035\000\113\000\036\000\054\000\037\000\053\000\038\000\052\000\
\\039\000\113\000\000\000\
\\001\000\004\000\114\000\008\000\114\000\018\000\114\000\025\000\114\000\
\\027\000\114\000\028\000\114\000\029\000\114\000\030\000\114\000\
\\031\000\114\000\032\000\114\000\033\000\114\000\034\000\114\000\
\\035\000\114\000\036\000\054\000\037\000\053\000\038\000\052\000\
\\039\000\114\000\000\000\
\\001\000\004\000\115\000\008\000\115\000\018\000\115\000\025\000\115\000\
\\027\000\115\000\028\000\115\000\029\000\115\000\030\000\115\000\
\\031\000\115\000\032\000\115\000\033\000\115\000\034\000\115\000\
\\035\000\115\000\036\000\054\000\037\000\053\000\038\000\052\000\
\\039\000\115\000\000\000\
\\001\000\004\000\116\000\008\000\116\000\018\000\116\000\025\000\116\000\
\\027\000\116\000\028\000\116\000\029\000\116\000\030\000\116\000\
\\031\000\116\000\032\000\116\000\033\000\116\000\034\000\116\000\
\\035\000\116\000\036\000\116\000\037\000\116\000\038\000\116\000\
\\039\000\116\000\000\000\
\\001\000\004\000\117\000\008\000\117\000\018\000\117\000\025\000\117\000\
\\027\000\117\000\028\000\117\000\029\000\117\000\030\000\117\000\
\\031\000\117\000\032\000\117\000\033\000\117\000\034\000\117\000\
\\035\000\117\000\036\000\117\000\037\000\117\000\038\000\117\000\
\\039\000\117\000\000\000\
\\001\000\004\000\118\000\008\000\118\000\018\000\118\000\025\000\118\000\
\\027\000\118\000\028\000\118\000\029\000\118\000\030\000\118\000\
\\031\000\118\000\032\000\118\000\033\000\118\000\034\000\118\000\
\\035\000\118\000\036\000\118\000\037\000\118\000\038\000\118\000\
\\039\000\118\000\000\000\
\\001\000\004\000\119\000\008\000\119\000\018\000\119\000\025\000\119\000\
\\027\000\119\000\028\000\119\000\029\000\119\000\030\000\119\000\
\\031\000\119\000\032\000\119\000\033\000\119\000\034\000\119\000\
\\035\000\119\000\036\000\119\000\037\000\119\000\038\000\119\000\
\\039\000\119\000\000\000\
\\001\000\004\000\120\000\008\000\120\000\018\000\120\000\025\000\120\000\
\\027\000\120\000\028\000\120\000\029\000\120\000\030\000\120\000\
\\031\000\120\000\032\000\120\000\033\000\120\000\034\000\120\000\
\\035\000\120\000\036\000\120\000\037\000\120\000\038\000\120\000\
\\039\000\120\000\000\000\
\\001\000\004\000\121\000\008\000\121\000\018\000\121\000\025\000\121\000\
\\027\000\121\000\028\000\121\000\029\000\121\000\030\000\121\000\
\\031\000\121\000\032\000\121\000\033\000\121\000\034\000\121\000\
\\035\000\121\000\036\000\121\000\037\000\121\000\038\000\121\000\
\\039\000\121\000\000\000\
\\001\000\004\000\122\000\008\000\122\000\018\000\122\000\025\000\122\000\
\\027\000\122\000\028\000\122\000\029\000\122\000\030\000\122\000\
\\031\000\122\000\032\000\122\000\033\000\122\000\034\000\122\000\
\\035\000\122\000\036\000\122\000\037\000\122\000\038\000\122\000\
\\039\000\122\000\000\000\
\\001\000\004\000\123\000\008\000\123\000\018\000\123\000\025\000\123\000\
\\027\000\051\000\039\000\123\000\000\000\
\\001\000\004\000\124\000\008\000\124\000\018\000\124\000\025\000\124\000\
\\027\000\051\000\039\000\124\000\000\000\
\\001\000\004\000\125\000\008\000\125\000\018\000\125\000\025\000\125\000\
\\027\000\125\000\039\000\125\000\000\000\
\\001\000\004\000\126\000\008\000\126\000\018\000\126\000\025\000\126\000\
\\027\000\126\000\039\000\126\000\000\000\
\\001\000\004\000\127\000\008\000\127\000\018\000\127\000\025\000\127\000\
\\027\000\127\000\039\000\127\000\000\000\
\\001\000\004\000\128\000\008\000\128\000\018\000\128\000\025\000\128\000\
\\027\000\128\000\039\000\128\000\000\000\
\\001\000\004\000\129\000\008\000\129\000\018\000\120\000\025\000\120\000\
\\027\000\129\000\028\000\120\000\029\000\120\000\030\000\120\000\
\\031\000\120\000\032\000\120\000\033\000\120\000\034\000\120\000\
\\035\000\120\000\036\000\120\000\037\000\120\000\038\000\120\000\
\\039\000\129\000\000\000\
\\001\000\004\000\130\000\008\000\130\000\018\000\130\000\025\000\130\000\
\\027\000\130\000\034\000\058\000\035\000\057\000\039\000\130\000\000\000\
\\001\000\004\000\131\000\008\000\131\000\018\000\131\000\025\000\131\000\
\\027\000\131\000\034\000\058\000\035\000\057\000\039\000\131\000\000\000\
\\001\000\004\000\132\000\008\000\132\000\018\000\132\000\025\000\132\000\
\\027\000\132\000\034\000\058\000\035\000\057\000\039\000\132\000\000\000\
\\001\000\004\000\133\000\008\000\133\000\018\000\133\000\025\000\133\000\
\\027\000\133\000\034\000\058\000\035\000\057\000\039\000\133\000\000\000\
\\001\000\004\000\134\000\008\000\134\000\018\000\134\000\025\000\134\000\
\\027\000\134\000\034\000\058\000\035\000\057\000\039\000\134\000\000\000\
\\001\000\004\000\135\000\008\000\135\000\018\000\135\000\025\000\135\000\
\\027\000\135\000\034\000\058\000\035\000\057\000\039\000\135\000\000\000\
\\001\000\004\000\136\000\008\000\136\000\018\000\136\000\025\000\136\000\
\\027\000\136\000\039\000\136\000\000\000\
\\001\000\004\000\137\000\008\000\137\000\018\000\137\000\025\000\137\000\
\\027\000\137\000\039\000\137\000\000\000\
\\001\000\004\000\069\000\039\000\055\000\000\000\
\\001\000\005\000\091\000\000\000\
\\001\000\006\000\093\000\000\000\
\\001\000\008\000\056\000\039\000\055\000\000\000\
\\001\000\009\000\104\000\021\000\104\000\040\000\104\000\000\000\
\\001\000\009\000\105\000\021\000\105\000\040\000\105\000\000\000\
\\001\000\009\000\090\000\000\000\
\\001\000\010\000\046\000\011\000\045\000\000\000\
\\001\000\012\000\099\000\020\000\099\000\000\000\
\\001\000\012\000\009\000\020\000\098\000\000\000\
\\001\000\013\000\003\000\000\000\
\\001\000\014\000\038\000\015\000\037\000\023\000\036\000\024\000\035\000\
\\026\000\034\000\000\000\
\\001\000\016\000\005\000\000\000\
\\001\000\017\000\102\000\000\000\
\\001\000\017\000\103\000\000\000\
\\001\000\017\000\138\000\018\000\138\000\022\000\138\000\000\000\
\\001\000\017\000\024\000\000\000\
\\001\000\018\000\100\000\000\000\
\\001\000\018\000\101\000\000\000\
\\001\000\018\000\106\000\000\000\
\\001\000\018\000\107\000\000\000\
\\001\000\018\000\108\000\034\000\058\000\035\000\057\000\000\000\
\\001\000\018\000\109\000\000\000\
\\001\000\018\000\110\000\000\000\
\\001\000\018\000\111\000\028\000\064\000\029\000\063\000\030\000\062\000\
\\031\000\061\000\032\000\060\000\033\000\059\000\034\000\058\000\
\\035\000\057\000\000\000\
\\001\000\018\000\112\000\039\000\055\000\000\000\
\\001\000\018\000\026\000\000\000\
\\001\000\018\000\071\000\000\000\
\\001\000\020\000\097\000\000\000\
\\001\000\020\000\011\000\000\000\
\\001\000\021\000\072\000\000\000\
\\001\000\022\000\025\000\000\000\
\\001\000\023\000\036\000\024\000\042\000\000\000\
\\001\000\025\000\087\000\039\000\055\000\000\000\
\\001\000\025\000\088\000\028\000\064\000\029\000\063\000\030\000\062\000\
\\031\000\061\000\032\000\060\000\033\000\059\000\034\000\058\000\
\\035\000\057\000\000\000\
\\001\000\025\000\088\000\034\000\058\000\035\000\057\000\000\000\
\\001\000\028\000\064\000\029\000\063\000\030\000\062\000\031\000\061\000\
\\032\000\060\000\033\000\059\000\034\000\058\000\035\000\057\000\000\000\
\\001\000\040\000\000\000\000\000\
\\001\000\040\000\095\000\000\000\
\\001\000\040\000\096\000\000\000\
\"
val actionRowNumbers =
"\037\000\000\000\039\000\036\000\
\\056\000\036\000\065\000\041\000\
\\066\000\001\000\055\000\041\000\
\\043\000\042\000\058\000\053\000\
\\032\000\038\000\038\000\059\000\
\\000\000\040\000\034\000\038\000\
\\056\000\013\000\015\000\004\000\
\\008\000\030\000\063\000\018\000\
\\038\000\038\000\059\000\017\000\
\\016\000\027\000\048\000\009\000\
\\059\000\047\000\054\000\045\000\
\\044\000\046\000\052\000\051\000\
\\057\000\038\000\059\000\059\000\
\\059\000\038\000\056\000\059\000\
\\059\000\059\000\059\000\059\000\
\\059\000\059\000\059\000\026\000\
\\060\000\061\000\011\000\000\000\
\\062\000\035\000\031\000\014\000\
\\007\000\006\000\005\000\012\000\
\\033\000\003\000\002\000\024\000\
\\023\000\022\000\021\000\020\000\
\\019\000\025\000\010\000\028\000\
\\050\000\000\000\029\000\049\000\
\\064\000"
val gotoT =
"\
\\001\000\092\000\000\000\
\\002\000\002\000\000\000\
\\000\000\
\\003\000\006\000\004\000\005\000\005\000\004\000\000\000\
\\006\000\008\000\000\000\
\\004\000\005\000\005\000\010\000\000\000\
\\000\000\
\\002\000\013\000\007\000\012\000\015\000\011\000\000\000\
\\000\000\
\\002\000\013\000\014\000\015\000\015\000\014\000\000\000\
\\000\000\
\\002\000\013\000\007\000\021\000\015\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\031\000\008\000\030\000\009\000\029\000\010\000\028\000\
\\011\000\027\000\012\000\026\000\013\000\025\000\000\000\
\\002\000\031\000\008\000\030\000\009\000\037\000\010\000\028\000\
\\011\000\027\000\012\000\026\000\013\000\025\000\000\000\
\\002\000\039\000\008\000\038\000\010\000\028\000\011\000\027\000\000\000\
\\002\000\013\000\015\000\041\000\000\000\
\\000\000\
\\017\000\042\000\000\000\
\\002\000\031\000\008\000\047\000\009\000\046\000\010\000\028\000\
\\011\000\027\000\012\000\026\000\013\000\025\000\016\000\045\000\000\000\
\\006\000\048\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\031\000\008\000\030\000\010\000\028\000\011\000\027\000\
\\012\000\063\000\000\000\
\\002\000\031\000\008\000\065\000\009\000\064\000\010\000\028\000\
\\011\000\027\000\012\000\026\000\013\000\025\000\000\000\
\\002\000\039\000\010\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\039\000\008\000\068\000\010\000\028\000\011\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\031\000\008\000\030\000\010\000\028\000\011\000\027\000\
\\012\000\071\000\000\000\
\\002\000\039\000\010\000\072\000\000\000\
\\002\000\039\000\010\000\073\000\000\000\
\\002\000\039\000\010\000\074\000\000\000\
\\002\000\031\000\008\000\030\000\010\000\028\000\011\000\027\000\
\\012\000\026\000\013\000\075\000\000\000\
\\006\000\076\000\000\000\
\\002\000\039\000\010\000\028\000\011\000\077\000\000\000\
\\002\000\039\000\010\000\028\000\011\000\078\000\000\000\
\\002\000\039\000\008\000\079\000\010\000\028\000\011\000\027\000\000\000\
\\002\000\039\000\008\000\080\000\010\000\028\000\011\000\027\000\000\000\
\\002\000\039\000\008\000\081\000\010\000\028\000\011\000\027\000\000\000\
\\002\000\039\000\008\000\082\000\010\000\028\000\011\000\027\000\000\000\
\\002\000\039\000\008\000\083\000\010\000\028\000\011\000\027\000\000\000\
\\002\000\039\000\008\000\084\000\010\000\028\000\011\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\087\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\019\000\090\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 93
val numrules = 44
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
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | cmdSeq2 of unit ->  (string) | cmdSeq1 of unit ->  (string)
 | type of unit ->  (string) | exp of unit ->  (string)
 | var of unit ->  (string) | cmd of unit ->  (CMD)
 | boolTerm of unit ->  (BOOL) | boolFactor of unit ->  (BOOL)
 | intTerm of unit ->  (INT) | intFactor of unit ->  (INT)
 | boolExp of unit ->  (BEXP) | intExp of unit ->  (IEXP)
 | varList of unit ->  (string list) | cmdSeq of unit ->  (string)
 | decSeq of unit ->  (string) | dec of unit ->  (DEC)
 | block of unit ->  (BLK) | id of unit ->  (string)
 | program of unit ->  (PROG)
end
type svalue = MlyValue.svalue
type result = PROG
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 39) => true | _ => false
val showTerminal =
fn (T 0) => "READ"
  | (T 1) => "WRITE"
  | (T 2) => "IF"
  | (T 3) => "THEN"
  | (T 4) => "ELSE"
  | (T 5) => "ENDIF"
  | (T 6) => "WHILE"
  | (T 7) => "DO"
  | (T 8) => "ENDWH"
  | (T 9) => "INT"
  | (T 10) => "BOOL"
  | (T 11) => "VAR"
  | (T 12) => "PROGRAM"
  | (T 13) => "TT"
  | (T 14) => "FF"
  | (T 15) => "START"
  | (T 16) => "TYPEOF"
  | (T 17) => "EOS"
  | (T 18) => "COMMA"
  | (T 19) => "LSPAR"
  | (T 20) => "RSPAR"
  | (T 21) => "ASSIGN"
  | (T 22) => "UMINUS"
  | (T 23) => "LPAR"
  | (T 24) => "RPAR"
  | (T 25) => "NOT"
  | (T 26) => "AND"
  | (T 27) => "LT"
  | (T 28) => "LEQ"
  | (T 29) => "EQ"
  | (T 30) => "GT"
  | (T 31) => "GEQ"
  | (T 32) => "NEQ"
  | (T 33) => "PLUS"
  | (T 34) => "MINUS"
  | (T 35) => "TIMES"
  | (T 36) => "DIV"
  | (T 37) => "MOD"
  | (T 38) => "OR"
  | (T 39) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.block block1, _, block1right)) :: _ :: ( _,
 ( MlyValue.id id1, _, _)) :: ( _, ( _, PROGRAM1left, _)) :: rest671))
 => let val  result = MlyValue.program (fn _ => let val  (id as id1) =
 id1 ()
 val  (block as block1) = block1 ()
 in ((PROG(id,block)))
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.cmdSeq cmdSeq1, _, cmdSeq1right)) :: ( _, ( 
MlyValue.decSeq decSeq1, decSeq1left, _)) :: rest671)) => let val  
result = MlyValue.block (fn _ => let val  (decSeq as decSeq1) = 
decSeq1 ()
 val  (cmdSeq as cmdSeq1) = cmdSeq1 ()
 in ((BLK(decSeq,cmdSeq)))
end)
 in ( LrTable.NT 2, ( result, decSeq1left, cmdSeq1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.decSeq decSeq1, _, decSeq1right)) :: ( _, ( 
MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.decSeq (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decSeq as decSeq1) = decSeq1 ()
 in ((dec::decSeq))
end)
 in ( LrTable.NT 4, ( result, dec1left, decSeq1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.decSeq (fn _ => ([])
)
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, EOS1right)) :: ( _, ( MlyValue.type type1, _, _
)) :: _ :: ( _, ( MlyValue.varList varList1, _, _)) :: ( _, ( _, 
VAR1left, _)) :: rest671)) => let val  result = MlyValue.dec (fn _ =>
 let val  (varList as varList1) = varList1 ()
 val  type1 = type1 ()
 in ((DEC(varList)))
end)
 in ( LrTable.NT 3, ( result, VAR1left, EOS1right), rest671)
end
|  ( 5, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.type (fn _ => ((INT)))
 in ( LrTable.NT 16, ( result, INT1left, INT1right), rest671)
end
|  ( 6, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.type (fn _ => ((BOOL)))
 in ( LrTable.NT 16, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.varList varList1, _, varList1right)) :: ( _,
 ( MlyValue.var var1, var1left, _)) :: rest671)) => let val  result = 
MlyValue.varList (fn _ => let val  (var as var1) = var1 ()
 val  (varList as varList1) = varList1 ()
 in ((var::varList))
end)
 in ( LrTable.NT 6, ( result, var1left, varList1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.varList (fn _ => ([]
))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( _, _, RSPAR1right)) :: ( _, ( MlyValue.cmdSeq cmdSeq1
, _, _)) :: _ :: ( _, ( MlyValue.cmd cmd1, _, _)) :: ( _, ( _, 
LSPAR1left, _)) :: rest671)) => let val  result = MlyValue.cmdSeq (fn
 _ => let val  (cmd as cmd1) = cmd1 ()
 val  (cmdSeq as cmdSeq1) = cmdSeq1 ()
 in ((cmd::cmdSeq))
end)
 in ( LrTable.NT 5, ( result, LSPAR1left, RSPAR1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RSPAR1right)) :: ( _, ( _, LSPAR1left, _)) :: 
rest671)) => let val  result = MlyValue.cmdSeq (fn _ => ([]))
 in ( LrTable.NT 5, ( result, LSPAR1left, RSPAR1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.var var1, var1left, _)) :: rest671)) => let val  result = 
MlyValue.cmd (fn _ => let val  (var as var1) = var1 ()
 val  (exp as exp1) = exp1 ()
 in ((SET(var,exp)))
end)
 in ( LrTable.NT 13, ( result, var1left, exp1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.var var1, _, var1right)) :: ( _, ( _, 
READ1left, _)) :: rest671)) => let val  result = MlyValue.cmd (fn _ =>
 let val  (var as var1) = var1 ()
 in ((CMD(var)))
end)
 in ( LrTable.NT 13, ( result, READ1left, var1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.intExp intExp1, _, intExp1right)) :: ( _, (
 _, WRITE1left, _)) :: rest671)) => let val  result = MlyValue.cmd (fn
 _ => let val  (intExp as intExp1) = intExp1 ()
 in ((CMD(intExp)))
end)
 in ( LrTable.NT 13, ( result, WRITE1left, intExp1right), rest671)
end
|  ( 14, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.cmdSeq2 
cmdSeq21, _, _)) :: _ :: ( _, ( MlyValue.cmdSeq1 cmdSeq11, _, _)) :: _
 :: ( _, ( MlyValue.boolExp boolExp1, _, _)) :: ( _, ( _, IF1left, _))
 :: rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (
boolExp as boolExp1) = boolExp1 ()
 val  (cmdSeq1 as cmdSeq11) = cmdSeq11 ()
 val  (cmdSeq2 as cmdSeq21) = cmdSeq21 ()
 in ((ITE(boolExp,cmdSeq1,cmdSeq2)))
end)
 in ( LrTable.NT 13, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 15, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.cmdSeq 
cmdSeq1, _, _)) :: _ :: ( _, ( MlyValue.boolExp boolExp1, _, _)) :: (
 _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.cmd (fn _ => let val  (boolExp as boolExp1) = boolExp1 ()
 val  (cmdSeq as cmdSeq1) = cmdSeq1 ()
 in ((WH(boolExp,cmdSeq)))
end)
 in ( LrTable.NT 13, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.intExp intExp1, intExp1left, intExp1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
intExp as intExp1) = intExp1 ()
 in ((IEXP(intExp)))
end)
 in ( LrTable.NT 15, ( result, intExp1left, intExp1right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.boolExp boolExp1, boolExp1left, 
boolExp1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (boolExp as boolExp1) = boolExp1 ()
 in ((BEXP(boolExp)))
end)
 in ( LrTable.NT 15, ( result, boolExp1left, boolExp1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.intTerm intTerm1, _, intTerm1right)) :: _
 :: ( _, ( MlyValue.intExp intExp1, intExp1left, _)) :: rest671)) =>
 let val  result = MlyValue.intExp (fn _ => let val  (intExp as 
intExp1) = intExp1 ()
 val  (intTerm as intTerm1) = intTerm1 ()
 in ((PLUS(intExp,intTerm)))
end)
 in ( LrTable.NT 7, ( result, intExp1left, intTerm1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.intTerm intTerm1, _, intTerm1right)) :: _
 :: ( _, ( MlyValue.intExp intExp1, intExp1left, _)) :: rest671)) =>
 let val  result = MlyValue.intExp (fn _ => let val  (intExp as 
intExp1) = intExp1 ()
 val  (intTerm as intTerm1) = intTerm1 ()
 in ((MINUS(intExp,intTerm)))
end)
 in ( LrTable.NT 7, ( result, intExp1left, intTerm1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.intTerm intTerm1, intTerm1left, 
intTerm1right)) :: rest671)) => let val  result = MlyValue.intExp (fn
 _ => let val  (intTerm as intTerm1) = intTerm1 ()
 in ((intTerm))
end)
 in ( LrTable.NT 7, ( result, intTerm1left, intTerm1right), rest671)

end
|  ( 21, ( ( _, ( MlyValue.intFactor intFactor1, _, intFactor1right))
 :: _ :: ( _, ( MlyValue.intTerm intTerm1, intTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.intTerm (fn _ => let val  (
intTerm as intTerm1) = intTerm1 ()
 val  (intFactor as intFactor1) = intFactor1 ()
 in ((TIMES(intTerm,intFactor)))
end)
 in ( LrTable.NT 10, ( result, intTerm1left, intFactor1right), rest671
)
end
|  ( 22, ( ( _, ( MlyValue.intFactor intFactor1, _, intFactor1right))
 :: _ :: ( _, ( MlyValue.intTerm intTerm1, intTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.intTerm (fn _ => let val  (
intTerm as intTerm1) = intTerm1 ()
 val  (intFactor as intFactor1) = intFactor1 ()
 in ((DIV(intTerm,intFactor)))
end)
 in ( LrTable.NT 10, ( result, intTerm1left, intFactor1right), rest671
)
end
|  ( 23, ( ( _, ( MlyValue.intFactor intFactor1, _, intFactor1right))
 :: _ :: ( _, ( MlyValue.intTerm intTerm1, intTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.intTerm (fn _ => let val  (
intTerm as intTerm1) = intTerm1 ()
 val  (intFactor as intFactor1) = intFactor1 ()
 in ((MOD(intTerm,intFactor)))
end)
 in ( LrTable.NT 10, ( result, intTerm1left, intFactor1right), rest671
)
end
|  ( 24, ( ( _, ( MlyValue.intFactor intFactor1, intFactor1left, 
intFactor1right)) :: rest671)) => let val  result = MlyValue.intTerm
 (fn _ => let val  (intFactor as intFactor1) = intFactor1 ()
 in ((intFactor))
end)
 in ( LrTable.NT 10, ( result, intFactor1left, intFactor1right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.id id1, id1left, id1right)) :: rest671)) =>
 let val  result = MlyValue.intFactor (fn _ => let val  (id as id1) = 
id1 ()
 in ((id))
end)
 in ( LrTable.NT 9, ( result, id1left, id1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.intExp intExp1
, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result =
 MlyValue.intFactor (fn _ => let val  (intExp as intExp1) = intExp1 ()
 in ((intExp))
end)
 in ( LrTable.NT 9, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.intFactor intFactor1, _, intFactor1right))
 :: ( _, ( _, UMINUS1left, _)) :: rest671)) => let val  result = 
MlyValue.intFactor (fn _ => let val  (intFactor as intFactor1) = 
intFactor1 ()
 in ((UMINUS(intFactor)))
end)
 in ( LrTable.NT 9, ( result, UMINUS1left, intFactor1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.boolTerm boolTerm1, _, boolTerm1right)) ::
 _ :: ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671))
 => let val  result = MlyValue.boolExp (fn _ => let val  (boolExp as 
boolExp1) = boolExp1 ()
 val  (boolTerm as boolTerm1) = boolTerm1 ()
 in ((OR(boolExp,boolTerm)))
end)
 in ( LrTable.NT 8, ( result, boolExp1left, boolTerm1right), rest671)

end
|  ( 29, ( ( _, ( MlyValue.boolTerm boolTerm1, boolTerm1left, 
boolTerm1right)) :: rest671)) => let val  result = MlyValue.boolExp
 (fn _ => let val  (boolTerm as boolTerm1) = boolTerm1 ()
 in ((boolTerm))
end)
 in ( LrTable.NT 8, ( result, boolTerm1left, boolTerm1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.boolFactor boolFactor1, _, boolFactor1right
)) :: _ :: ( _, ( MlyValue.boolTerm boolTerm1, boolTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.boolTerm (fn _ => let val  (
boolTerm as boolTerm1) = boolTerm1 ()
 val  (boolFactor as boolFactor1) = boolFactor1 ()
 in ((AND(boolTerm,boolFactor)))
end)
 in ( LrTable.NT 12, ( result, boolTerm1left, boolFactor1right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.boolFactor boolFactor1, boolFactor1left, 
boolFactor1right)) :: rest671)) => let val  result = MlyValue.boolTerm
 (fn _ => let val  (boolFactor as boolFactor1) = boolFactor1 ()
 in ((boolFactor))
end)
 in ( LrTable.NT 12, ( result, boolFactor1left, boolFactor1right), 
rest671)
end
|  ( 32, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.boolFactor (fn _ => ((TT)))
 in ( LrTable.NT 11, ( result, TT1left, TT1right), rest671)
end
|  ( 33, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.boolFactor (fn _ => ((FF)))
 in ( LrTable.NT 11, ( result, FF1left, FF1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.id id1, id1left, id1right)) :: rest671)) =>
 let val  result = MlyValue.boolFactor (fn _ => let val  (id as id1) =
 id1 ()
 in ((id))
end)
 in ( LrTable.NT 11, ( result, id1left, id1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.intExp intExp2, _, intExp2right)) :: _ :: (
 _, ( MlyValue.intExp intExp1, intExp1left, _)) :: rest671)) => let
 val  result = MlyValue.boolFactor (fn _ => let val  (intExp as 
intExp1) = intExp1 ()
 val  intExp2 = intExp2 ()
 in ((LT(intExp,intExp)))
end)
 in ( LrTable.NT 11, ( result, intExp1left, intExp2right), rest671)

end
|  ( 36, ( ( _, ( MlyValue.intExp intExp2, _, intExp2right)) :: _ :: (
 _, ( MlyValue.intExp intExp1, intExp1left, _)) :: rest671)) => let
 val  result = MlyValue.boolFactor (fn _ => let val  (intExp as 
intExp1) = intExp1 ()
 val  intExp2 = intExp2 ()
 in ((LEQ(intExp,intExp)))
end)
 in ( LrTable.NT 11, ( result, intExp1left, intExp2right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.intExp intExp2, _, intExp2right)) :: _ :: (
 _, ( MlyValue.intExp intExp1, intExp1left, _)) :: rest671)) => let
 val  result = MlyValue.boolFactor (fn _ => let val  (intExp as 
intExp1) = intExp1 ()
 val  intExp2 = intExp2 ()
 in ((EQ(intExp,intExp)))
end)
 in ( LrTable.NT 11, ( result, intExp1left, intExp2right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.intExp intExp2, _, intExp2right)) :: _ :: (
 _, ( MlyValue.intExp intExp1, intExp1left, _)) :: rest671)) => let
 val  result = MlyValue.boolFactor (fn _ => let val  (intExp as 
intExp1) = intExp1 ()
 val  intExp2 = intExp2 ()
 in ((GT(intExp,intExp)))
end)
 in ( LrTable.NT 11, ( result, intExp1left, intExp2right), rest671)

end
|  ( 39, ( ( _, ( MlyValue.intExp intExp2, _, intExp2right)) :: _ :: (
 _, ( MlyValue.intExp intExp1, intExp1left, _)) :: rest671)) => let
 val  result = MlyValue.boolFactor (fn _ => let val  (intExp as 
intExp1) = intExp1 ()
 val  intExp2 = intExp2 ()
 in ((GEQ(intExp,intExp)))
end)
 in ( LrTable.NT 11, ( result, intExp1left, intExp2right), rest671)

end
|  ( 40, ( ( _, ( MlyValue.intExp intExp2, _, intExp2right)) :: _ :: (
 _, ( MlyValue.intExp intExp1, intExp1left, _)) :: rest671)) => let
 val  result = MlyValue.boolFactor (fn _ => let val  (intExp as 
intExp1) = intExp1 ()
 val  intExp2 = intExp2 ()
 in ((NEQ(intExp,intExp)))
end)
 in ( LrTable.NT 11, ( result, intExp1left, intExp2right), rest671)

end
|  ( 41, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.boolExp 
boolExp1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  
result = MlyValue.boolFactor (fn _ => let val  (boolExp as boolExp1) =
 boolExp1 ()
 in ((boolExp))
end)
 in ( LrTable.NT 11, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.boolFactor boolFactor1, _, boolFactor1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.boolFactor (fn _ => let val  (boolFactor as boolFactor1) = 
boolFactor1 ()
 in ((NOT(boolFactor)))
end)
 in ( LrTable.NT 11, ( result, NOT1left, boolFactor1right), rest671)

end
|  ( 43, ( ( _, ( MlyValue.id id1, id1left, id1right)) :: rest671)) =>
 let val  result = MlyValue.var (fn _ => let val  (id as id1) = id1 ()
 in ((id))
end)
 in ( LrTable.NT 14, ( result, id1left, id1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun START (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPEOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun EOS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LSPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun RSPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
end
end
