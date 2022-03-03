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
open AST;

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\018\000\005\000\017\000\006\000\016\000\007\000\015\000\
\\014\000\068\000\016\000\014\000\017\000\013\000\019\000\012\000\
\\035\000\011\000\000\000\
\\001\000\001\000\018\000\005\000\017\000\006\000\016\000\007\000\035\000\
\\016\000\014\000\017\000\013\000\019\000\012\000\035\000\011\000\000\000\
\\001\000\002\000\072\000\003\000\072\000\004\000\072\000\011\000\072\000\
\\018\000\072\000\020\000\072\000\021\000\072\000\022\000\072\000\
\\023\000\072\000\024\000\072\000\025\000\072\000\026\000\072\000\
\\027\000\072\000\028\000\072\000\029\000\072\000\030\000\072\000\
\\031\000\072\000\032\000\072\000\036\000\072\000\037\000\072\000\000\000\
\\001\000\002\000\073\000\003\000\073\000\004\000\073\000\011\000\073\000\
\\018\000\073\000\020\000\073\000\021\000\073\000\022\000\073\000\
\\023\000\073\000\024\000\073\000\025\000\073\000\026\000\073\000\
\\027\000\073\000\028\000\073\000\029\000\073\000\030\000\073\000\
\\031\000\073\000\032\000\073\000\036\000\073\000\037\000\073\000\000\000\
\\001\000\002\000\074\000\003\000\074\000\004\000\074\000\011\000\074\000\
\\018\000\074\000\020\000\074\000\021\000\074\000\022\000\074\000\
\\023\000\074\000\024\000\074\000\025\000\074\000\026\000\074\000\
\\027\000\074\000\028\000\074\000\029\000\074\000\030\000\074\000\
\\031\000\074\000\032\000\074\000\036\000\074\000\037\000\074\000\000\000\
\\001\000\002\000\075\000\003\000\075\000\004\000\075\000\011\000\075\000\
\\018\000\075\000\020\000\075\000\021\000\075\000\022\000\075\000\
\\023\000\075\000\024\000\075\000\025\000\075\000\026\000\075\000\
\\027\000\075\000\028\000\075\000\029\000\075\000\030\000\075\000\
\\031\000\075\000\032\000\075\000\036\000\075\000\037\000\075\000\000\000\
\\001\000\002\000\076\000\003\000\076\000\004\000\076\000\011\000\076\000\
\\018\000\076\000\020\000\076\000\021\000\076\000\022\000\076\000\
\\023\000\076\000\024\000\076\000\025\000\076\000\026\000\076\000\
\\027\000\076\000\028\000\076\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\076\000\036\000\076\000\037\000\076\000\000\000\
\\001\000\002\000\077\000\003\000\077\000\004\000\077\000\011\000\077\000\
\\018\000\077\000\020\000\077\000\021\000\077\000\022\000\077\000\
\\023\000\077\000\024\000\077\000\025\000\077\000\026\000\077\000\
\\027\000\077\000\028\000\077\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\077\000\036\000\077\000\037\000\077\000\000\000\
\\001\000\002\000\078\000\003\000\078\000\004\000\078\000\011\000\078\000\
\\018\000\078\000\020\000\078\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\078\000\036\000\078\000\037\000\078\000\000\000\
\\001\000\002\000\079\000\003\000\079\000\004\000\079\000\011\000\079\000\
\\018\000\079\000\020\000\079\000\021\000\079\000\022\000\079\000\
\\023\000\079\000\024\000\079\000\025\000\079\000\026\000\079\000\
\\027\000\079\000\028\000\079\000\029\000\079\000\030\000\079\000\
\\031\000\079\000\032\000\079\000\036\000\079\000\037\000\079\000\000\000\
\\001\000\002\000\080\000\003\000\080\000\004\000\080\000\011\000\080\000\
\\018\000\080\000\020\000\080\000\021\000\080\000\022\000\080\000\
\\023\000\080\000\024\000\080\000\025\000\080\000\026\000\080\000\
\\027\000\080\000\028\000\080\000\029\000\080\000\030\000\080\000\
\\031\000\080\000\032\000\080\000\036\000\080\000\037\000\080\000\000\000\
\\001\000\002\000\081\000\003\000\081\000\004\000\081\000\011\000\081\000\
\\018\000\081\000\020\000\081\000\021\000\081\000\022\000\081\000\
\\023\000\081\000\024\000\081\000\025\000\081\000\026\000\081\000\
\\027\000\081\000\028\000\081\000\029\000\081\000\030\000\081\000\
\\031\000\081\000\032\000\081\000\036\000\081\000\037\000\081\000\000\000\
\\001\000\002\000\082\000\003\000\082\000\004\000\082\000\011\000\082\000\
\\018\000\082\000\020\000\082\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\082\000\036\000\082\000\037\000\082\000\000\000\
\\001\000\002\000\083\000\003\000\083\000\004\000\083\000\011\000\083\000\
\\018\000\083\000\020\000\083\000\021\000\083\000\022\000\083\000\
\\023\000\083\000\024\000\083\000\025\000\083\000\026\000\083\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\083\000\036\000\083\000\037\000\083\000\000\000\
\\001\000\002\000\084\000\003\000\084\000\004\000\084\000\011\000\084\000\
\\018\000\084\000\020\000\084\000\021\000\084\000\022\000\084\000\
\\023\000\084\000\024\000\084\000\025\000\084\000\026\000\084\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\084\000\036\000\084\000\037\000\084\000\000\000\
\\001\000\002\000\085\000\003\000\085\000\004\000\085\000\011\000\085\000\
\\018\000\085\000\020\000\085\000\021\000\031\000\022\000\030\000\
\\023\000\085\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\085\000\036\000\085\000\037\000\085\000\000\000\
\\001\000\002\000\086\000\003\000\086\000\004\000\086\000\011\000\086\000\
\\018\000\086\000\020\000\086\000\021\000\086\000\022\000\086\000\
\\023\000\086\000\024\000\086\000\025\000\086\000\026\000\086\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\086\000\036\000\086\000\037\000\086\000\000\000\
\\001\000\002\000\087\000\003\000\087\000\004\000\087\000\011\000\087\000\
\\018\000\087\000\020\000\087\000\021\000\087\000\022\000\087\000\
\\023\000\087\000\024\000\087\000\025\000\087\000\026\000\087\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\087\000\036\000\087\000\037\000\087\000\000\000\
\\001\000\002\000\088\000\003\000\088\000\004\000\088\000\011\000\088\000\
\\018\000\088\000\020\000\088\000\021\000\088\000\022\000\088\000\
\\023\000\088\000\024\000\088\000\025\000\088\000\026\000\088\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\088\000\036\000\088\000\037\000\088\000\000\000\
\\001\000\002\000\089\000\003\000\089\000\004\000\089\000\011\000\089\000\
\\018\000\089\000\020\000\089\000\021\000\089\000\022\000\089\000\
\\023\000\089\000\024\000\089\000\025\000\089\000\026\000\089\000\
\\027\000\089\000\028\000\089\000\029\000\089\000\030\000\089\000\
\\031\000\089\000\032\000\089\000\036\000\089\000\037\000\089\000\000\000\
\\001\000\002\000\090\000\003\000\090\000\004\000\090\000\011\000\090\000\
\\018\000\090\000\020\000\090\000\021\000\090\000\022\000\090\000\
\\023\000\090\000\024\000\090\000\025\000\090\000\026\000\090\000\
\\027\000\090\000\028\000\090\000\029\000\090\000\030\000\090\000\
\\031\000\090\000\032\000\090\000\036\000\090\000\037\000\090\000\000\000\
\\001\000\002\000\091\000\003\000\091\000\004\000\091\000\011\000\091\000\
\\018\000\091\000\020\000\091\000\021\000\091\000\022\000\091\000\
\\023\000\091\000\024\000\091\000\025\000\091\000\026\000\091\000\
\\027\000\091\000\028\000\091\000\029\000\091\000\030\000\091\000\
\\031\000\091\000\032\000\091\000\036\000\091\000\037\000\091\000\000\000\
\\001\000\002\000\092\000\003\000\092\000\004\000\092\000\011\000\092\000\
\\018\000\092\000\020\000\092\000\021\000\092\000\022\000\092\000\
\\023\000\092\000\024\000\092\000\025\000\092\000\026\000\092\000\
\\027\000\092\000\028\000\092\000\029\000\092\000\030\000\092\000\
\\031\000\092\000\032\000\092\000\036\000\092\000\037\000\092\000\000\000\
\\001\000\002\000\058\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\020\000\000\000\
\\001\000\003\000\062\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\020\000\000\000\
\\001\000\004\000\064\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\020\000\000\000\
\\001\000\007\000\004\000\000\000\
\\001\000\008\000\003\000\000\000\
\\001\000\009\000\005\000\000\000\
\\001\000\011\000\069\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\020\000\000\000\
\\001\000\011\000\070\000\000\000\
\\001\000\011\000\071\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\020\000\000\000\
\\001\000\011\000\074\000\015\000\039\000\020\000\074\000\021\000\074\000\
\\022\000\074\000\023\000\074\000\024\000\074\000\025\000\074\000\
\\026\000\074\000\027\000\074\000\028\000\074\000\029\000\074\000\
\\030\000\074\000\031\000\074\000\032\000\074\000\000\000\
\\001\000\011\000\019\000\000\000\
\\001\000\013\000\006\000\000\000\
\\001\000\014\000\067\000\000\000\
\\001\000\014\000\033\000\000\000\
\\001\000\018\000\056\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\027\000\025\000\028\000\024\000\029\000\023\000\030\000\022\000\
\\031\000\021\000\032\000\020\000\000\000\
\\001\000\020\000\032\000\021\000\031\000\022\000\030\000\023\000\029\000\
\\024\000\028\000\025\000\027\000\026\000\026\000\027\000\025\000\
\\028\000\024\000\029\000\023\000\030\000\022\000\031\000\021\000\
\\032\000\020\000\036\000\055\000\000\000\
\\001\000\020\000\032\000\021\000\031\000\022\000\030\000\023\000\029\000\
\\024\000\028\000\025\000\027\000\026\000\026\000\027\000\025\000\
\\028\000\024\000\029\000\023\000\030\000\022\000\031\000\021\000\
\\032\000\020\000\037\000\061\000\000\000\
\\001\000\033\000\000\000\000\000\
\\001\000\033\000\066\000\000\000\
\"
val actionRowNumbers =
"\027\000\026\000\028\000\034\000\
\\000\000\030\000\033\000\029\000\
\\036\000\001\000\001\000\001\000\
\\001\000\032\000\002\000\003\000\
\\001\000\000\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\041\000\
\\038\000\004\000\005\000\037\000\
\\019\000\001\000\023\000\035\000\
\\008\000\011\000\010\000\009\000\
\\007\000\006\000\018\000\017\000\
\\016\000\015\000\014\000\013\000\
\\012\000\001\000\020\000\031\000\
\\001\000\039\000\024\000\022\000\
\\001\000\025\000\021\000\040\000"
val gotoT =
"\
\\001\000\063\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\008\000\003\000\007\000\004\000\006\000\005\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\032\000\000\000\
\\003\000\034\000\000\000\
\\003\000\035\000\000\000\
\\003\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\038\000\000\000\
\\002\000\039\000\003\000\007\000\004\000\006\000\005\000\005\000\000\000\
\\003\000\040\000\000\000\
\\003\000\041\000\000\000\
\\003\000\042\000\000\000\
\\003\000\043\000\000\000\
\\003\000\044\000\000\000\
\\003\000\045\000\000\000\
\\003\000\046\000\000\000\
\\003\000\047\000\000\000\
\\003\000\048\000\000\000\
\\003\000\049\000\000\000\
\\003\000\050\000\000\000\
\\003\000\051\000\000\000\
\\003\000\052\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\055\000\000\000\
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
\\003\000\057\000\000\000\
\\000\000\
\\000\000\
\\003\000\058\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 64
val numrules = 27
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
 | ID of unit ->  (string) | CONST of unit ->  (bool)
 | NUM of unit ->  (int) | decl of unit ->  (id*AST.Exp)
 | cmd of unit ->  (AST.Stat) | exp of unit ->  (AST.Exp)
 | cmdSeq of unit ->  (AST.Stat list)
 | program of unit ->  (AST.Stat list)
end
type svalue = MlyValue.svalue
type result = AST.Stat list
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
fn (T 32) => true | _ => false
val showTerminal =
fn (T 0) => "IF"
  | (T 1) => "THEN"
  | (T 2) => "ELSE"
  | (T 3) => "ENDIF"
  | (T 4) => "NUM"
  | (T 5) => "CONST"
  | (T 6) => "ID"
  | (T 7) => "PROGRAM"
  | (T 8) => "START"
  | (T 9) => "TYPEOF"
  | (T 10) => "EOS"
  | (T 11) => "COMMA"
  | (T 12) => "LSPAR"
  | (T 13) => "RSPAR"
  | (T 14) => "ASSIGN"
  | (T 15) => "NEGATE"
  | (T 16) => "LPAR"
  | (T 17) => "RPAR"
  | (T 18) => "NOT"
  | (T 19) => "AND"
  | (T 20) => "LT"
  | (T 21) => "LEQ"
  | (T 22) => "EQ"
  | (T 23) => "GT"
  | (T 24) => "GEQ"
  | (T 25) => "NEQ"
  | (T 26) => "PLUS"
  | (T 27) => "MINUS"
  | (T 28) => "TIMES"
  | (T 29) => "DIV"
  | (T 30) => "MOD"
  | (T 31) => "OR"
  | (T 32) => "EOF"
  | (T 33) => "ILLCH"
  | (T 34) => "WHILE"
  | (T 35) => "DO"
  | (T 36) => "ENDWH"
  | (T 37) => "TT"
  | (T 38) => "FF"
  | (T 39) => "READ"
  | (T 40) => "WRITE"
  | (T 41) => "VAR"
  | (T 42) => "INT"
  | (T 43) => "BOOL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37)
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, RSPAR1right)) :: ( _, ( MlyValue.cmdSeq 
cmdSeq1, _, _)) :: _ :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, (
 _, PROGRAM1left, _)) :: rest671)) => let val  result = 
MlyValue.program (fn _ => let val  ID1 = ID1 ()
 val  (cmdSeq as cmdSeq1) = cmdSeq1 ()
 in (cmdSeq)
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, RSPAR1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.cmdSeq cmdSeq1, _, cmdSeq1right)) :: _ :: (
 _, ( MlyValue.cmd cmd1, cmd1left, _)) :: rest671)) => let val  result
 = MlyValue.cmdSeq (fn _ => let val  (cmd as cmd1) = cmd1 ()
 val  (cmdSeq as cmdSeq1) = cmdSeq1 ()
 in (cmd::cmdSeq)
end)
 in ( LrTable.NT 1, ( result, cmd1left, cmdSeq1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.cmdSeq (fn _ => ([])
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.cmd (fn _ => let val  (exp as exp1) = 
exp1 ()
 in (AST.EXP(exp))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.decl decl1, decl1left, decl1right)) :: 
rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (decl
 as decl1) = decl1 ()
 in (AST.SET(decl))
end)
 in ( LrTable.NT 3, ( result, decl1left, decl1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.decl (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((ID,exp))
end)
 in ( LrTable.NT 4, ( result, ID1left, exp1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (CONST
 as CONST1) = CONST1 ()
 in (AST.BoolExp(CONST))
end)
 in ( LrTable.NT 2, ( result, CONST1left, CONST1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 2, ( result, NUM1left, NUM1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (AST.UnExp(AST.NOT,exp))
end)
 in ( LrTable.NT 2, ( result, NOT1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.PLUS,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.MINUS,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.OR,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.TIMES,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.DIV,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.MOD,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.AND,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.LT,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.LEQ,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.EQ,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.GT,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.GEQ,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.BinExp(AST.NEQ,exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (exp as exp1) = exp1 ()
 in (AST.UnExp(AST.NEGATE,exp))
end)
 in ( LrTable.NT 2, ( result, NEGATE1left, exp1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.exp exp1, _, _
)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 2, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 25, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.exp exp3, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp2, _, _)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (AST.IfExp(exp1,exp2,exp3))
end)
 in ( LrTable.NT 2, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 26, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.exp exp2, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, WHILE1left,
 _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.WhileExp(exp1,exp2))
end)
 in ( LrTable.NT 2, ( result, WHILE1left, ENDWH1right), rest671)
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
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun START (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPEOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun EOS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LSPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun RSPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun ILLCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
end
end
