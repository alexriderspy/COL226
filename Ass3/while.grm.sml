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
\\001\000\001\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\008\000\024\000\015\000\097\000\017\000\023\000\018\000\022\000\
\\020\000\021\000\036\000\020\000\039\000\019\000\040\000\018\000\000\000\
\\001\000\001\000\028\000\005\000\027\000\006\000\026\000\007\000\025\000\
\\008\000\047\000\017\000\023\000\018\000\022\000\020\000\021\000\
\\036\000\020\000\000\000\
\\001\000\002\000\103\000\012\000\103\000\019\000\103\000\021\000\103\000\
\\022\000\103\000\023\000\103\000\024\000\103\000\025\000\103\000\
\\026\000\103\000\027\000\103\000\028\000\103\000\029\000\103\000\
\\030\000\103\000\031\000\103\000\032\000\103\000\033\000\103\000\
\\037\000\103\000\000\000\
\\001\000\002\000\104\000\012\000\104\000\019\000\104\000\021\000\104\000\
\\022\000\104\000\023\000\104\000\024\000\104\000\025\000\104\000\
\\026\000\104\000\027\000\104\000\028\000\104\000\029\000\104\000\
\\030\000\104\000\031\000\104\000\032\000\104\000\033\000\104\000\
\\037\000\104\000\000\000\
\\001\000\002\000\105\000\012\000\105\000\019\000\105\000\021\000\105\000\
\\022\000\105\000\023\000\105\000\024\000\105\000\025\000\105\000\
\\026\000\105\000\027\000\105\000\028\000\105\000\029\000\105\000\
\\030\000\105\000\031\000\105\000\032\000\105\000\033\000\105\000\
\\037\000\105\000\000\000\
\\001\000\002\000\106\000\012\000\106\000\019\000\106\000\021\000\106\000\
\\022\000\106\000\023\000\106\000\024\000\106\000\025\000\106\000\
\\026\000\106\000\027\000\106\000\028\000\106\000\029\000\106\000\
\\030\000\106\000\031\000\106\000\032\000\106\000\033\000\106\000\
\\037\000\106\000\000\000\
\\001\000\002\000\107\000\012\000\107\000\019\000\107\000\021\000\107\000\
\\022\000\107\000\023\000\107\000\024\000\107\000\025\000\107\000\
\\026\000\107\000\027\000\107\000\028\000\107\000\029\000\107\000\
\\030\000\107\000\031\000\107\000\032\000\107\000\033\000\107\000\
\\037\000\107\000\000\000\
\\001\000\002\000\108\000\012\000\108\000\019\000\108\000\021\000\108\000\
\\022\000\108\000\023\000\108\000\024\000\108\000\025\000\108\000\
\\026\000\108\000\027\000\108\000\028\000\108\000\029\000\108\000\
\\030\000\034\000\031\000\033\000\032\000\032\000\033\000\108\000\
\\037\000\108\000\000\000\
\\001\000\002\000\109\000\012\000\109\000\019\000\109\000\021\000\109\000\
\\022\000\109\000\023\000\109\000\024\000\109\000\025\000\109\000\
\\026\000\109\000\027\000\109\000\028\000\109\000\029\000\109\000\
\\030\000\034\000\031\000\033\000\032\000\032\000\033\000\109\000\
\\037\000\109\000\000\000\
\\001\000\002\000\110\000\012\000\110\000\019\000\110\000\021\000\110\000\
\\022\000\042\000\023\000\041\000\024\000\040\000\025\000\039\000\
\\026\000\038\000\027\000\037\000\028\000\036\000\029\000\035\000\
\\030\000\034\000\031\000\033\000\032\000\032\000\033\000\110\000\
\\037\000\110\000\000\000\
\\001\000\002\000\111\000\012\000\111\000\019\000\111\000\021\000\111\000\
\\022\000\111\000\023\000\111\000\024\000\111\000\025\000\111\000\
\\026\000\111\000\027\000\111\000\028\000\111\000\029\000\111\000\
\\030\000\111\000\031\000\111\000\032\000\111\000\033\000\111\000\
\\037\000\111\000\000\000\
\\001\000\002\000\112\000\012\000\112\000\019\000\112\000\021\000\112\000\
\\022\000\112\000\023\000\112\000\024\000\112\000\025\000\112\000\
\\026\000\112\000\027\000\112\000\028\000\112\000\029\000\112\000\
\\030\000\112\000\031\000\112\000\032\000\112\000\033\000\112\000\
\\037\000\112\000\000\000\
\\001\000\002\000\113\000\012\000\113\000\019\000\113\000\021\000\113\000\
\\022\000\113\000\023\000\113\000\024\000\113\000\025\000\113\000\
\\026\000\113\000\027\000\113\000\028\000\113\000\029\000\113\000\
\\030\000\113\000\031\000\113\000\032\000\113\000\033\000\113\000\
\\037\000\113\000\000\000\
\\001\000\002\000\114\000\012\000\114\000\019\000\114\000\021\000\114\000\
\\022\000\042\000\023\000\041\000\024\000\040\000\025\000\039\000\
\\026\000\038\000\027\000\037\000\028\000\036\000\029\000\035\000\
\\030\000\034\000\031\000\033\000\032\000\032\000\033\000\114\000\
\\037\000\114\000\000\000\
\\001\000\002\000\115\000\012\000\115\000\019\000\115\000\021\000\115\000\
\\022\000\115\000\023\000\115\000\024\000\115\000\025\000\115\000\
\\026\000\115\000\027\000\115\000\028\000\036\000\029\000\035\000\
\\030\000\034\000\031\000\033\000\032\000\032\000\033\000\115\000\
\\037\000\115\000\000\000\
\\001\000\002\000\116\000\012\000\116\000\019\000\116\000\021\000\116\000\
\\022\000\116\000\023\000\116\000\024\000\116\000\025\000\116\000\
\\026\000\116\000\027\000\116\000\028\000\036\000\029\000\035\000\
\\030\000\034\000\031\000\033\000\032\000\032\000\033\000\116\000\
\\037\000\116\000\000\000\
\\001\000\002\000\117\000\012\000\117\000\019\000\117\000\021\000\117\000\
\\022\000\042\000\023\000\041\000\024\000\117\000\025\000\039\000\
\\026\000\038\000\027\000\037\000\028\000\036\000\029\000\035\000\
\\030\000\034\000\031\000\033\000\032\000\032\000\033\000\117\000\
\\037\000\117\000\000\000\
\\001\000\002\000\118\000\012\000\118\000\019\000\118\000\021\000\118\000\
\\022\000\118\000\023\000\118\000\024\000\118\000\025\000\118\000\
\\026\000\118\000\027\000\118\000\028\000\036\000\029\000\035\000\
\\030\000\034\000\031\000\033\000\032\000\032\000\033\000\118\000\
\\037\000\118\000\000\000\
\\001\000\002\000\119\000\012\000\119\000\019\000\119\000\021\000\119\000\
\\022\000\119\000\023\000\119\000\024\000\119\000\025\000\119\000\
\\026\000\119\000\027\000\119\000\028\000\036\000\029\000\035\000\
\\030\000\034\000\031\000\033\000\032\000\032\000\033\000\119\000\
\\037\000\119\000\000\000\
\\001\000\002\000\120\000\012\000\120\000\019\000\120\000\021\000\120\000\
\\022\000\120\000\023\000\120\000\024\000\120\000\025\000\120\000\
\\026\000\120\000\027\000\120\000\028\000\036\000\029\000\035\000\
\\030\000\034\000\031\000\033\000\032\000\032\000\033\000\120\000\
\\037\000\120\000\000\000\
\\001\000\002\000\121\000\012\000\121\000\019\000\121\000\021\000\121\000\
\\022\000\121\000\023\000\121\000\024\000\121\000\025\000\121\000\
\\026\000\121\000\027\000\121\000\028\000\121\000\029\000\121\000\
\\030\000\121\000\031\000\121\000\032\000\121\000\033\000\121\000\
\\037\000\121\000\000\000\
\\001\000\002\000\122\000\012\000\122\000\019\000\122\000\021\000\122\000\
\\022\000\122\000\023\000\122\000\024\000\122\000\025\000\122\000\
\\026\000\122\000\027\000\122\000\028\000\122\000\029\000\122\000\
\\030\000\122\000\031\000\122\000\032\000\122\000\033\000\122\000\
\\037\000\122\000\000\000\
\\001\000\002\000\123\000\012\000\123\000\019\000\123\000\021\000\123\000\
\\022\000\123\000\023\000\123\000\024\000\123\000\025\000\123\000\
\\026\000\123\000\027\000\123\000\028\000\123\000\029\000\123\000\
\\030\000\123\000\031\000\123\000\032\000\123\000\033\000\123\000\
\\037\000\123\000\000\000\
\\001\000\002\000\124\000\012\000\124\000\019\000\124\000\021\000\124\000\
\\022\000\124\000\023\000\124\000\024\000\124\000\025\000\124\000\
\\026\000\124\000\027\000\124\000\028\000\124\000\029\000\124\000\
\\030\000\124\000\031\000\124\000\032\000\124\000\033\000\124\000\
\\037\000\124\000\000\000\
\\001\000\002\000\075\000\021\000\043\000\022\000\042\000\023\000\041\000\
\\024\000\040\000\025\000\039\000\026\000\038\000\027\000\037\000\
\\028\000\036\000\029\000\035\000\030\000\034\000\031\000\033\000\
\\032\000\032\000\033\000\031\000\000\000\
\\001\000\003\000\083\000\000\000\
\\001\000\004\000\087\000\000\000\
\\001\000\008\000\004\000\000\000\
\\001\000\008\000\012\000\000\000\
\\001\000\008\000\048\000\000\000\
\\001\000\009\000\003\000\000\000\
\\001\000\010\000\005\000\000\000\
\\001\000\011\000\094\000\000\000\
\\001\000\011\000\095\000\013\000\030\000\000\000\
\\001\000\011\000\029\000\000\000\
\\001\000\012\000\092\000\000\000\
\\001\000\012\000\093\000\000\000\
\\001\000\012\000\098\000\021\000\043\000\022\000\042\000\023\000\041\000\
\\024\000\040\000\025\000\039\000\026\000\038\000\027\000\037\000\
\\028\000\036\000\029\000\035\000\030\000\034\000\031\000\033\000\
\\032\000\032\000\033\000\031\000\000\000\
\\001\000\012\000\099\000\000\000\
\\001\000\012\000\100\000\000\000\
\\001\000\012\000\101\000\021\000\043\000\022\000\042\000\023\000\041\000\
\\024\000\040\000\025\000\039\000\026\000\038\000\027\000\037\000\
\\028\000\036\000\029\000\035\000\030\000\034\000\031\000\033\000\
\\032\000\032\000\033\000\031\000\000\000\
\\001\000\012\000\102\000\021\000\043\000\022\000\042\000\023\000\041\000\
\\024\000\040\000\025\000\039\000\026\000\038\000\027\000\037\000\
\\028\000\036\000\029\000\035\000\030\000\034\000\031\000\033\000\
\\032\000\032\000\033\000\031\000\000\000\
\\001\000\012\000\106\000\016\000\053\000\021\000\106\000\022\000\106\000\
\\023\000\106\000\024\000\106\000\025\000\106\000\026\000\106\000\
\\027\000\106\000\028\000\106\000\029\000\106\000\030\000\106\000\
\\031\000\106\000\032\000\106\000\033\000\106\000\000\000\
\\001\000\012\000\009\000\000\000\
\\001\000\012\000\044\000\000\000\
\\001\000\014\000\090\000\000\000\
\\001\000\014\000\091\000\041\000\008\000\000\000\
\\001\000\014\000\010\000\000\000\
\\001\000\014\000\076\000\000\000\
\\001\000\014\000\077\000\000\000\
\\001\000\014\000\084\000\000\000\
\\001\000\015\000\096\000\000\000\
\\001\000\015\000\045\000\000\000\
\\001\000\015\000\080\000\000\000\
\\001\000\015\000\081\000\000\000\
\\001\000\015\000\086\000\000\000\
\\001\000\019\000\073\000\021\000\043\000\022\000\042\000\023\000\041\000\
\\024\000\040\000\025\000\039\000\026\000\038\000\027\000\037\000\
\\028\000\036\000\029\000\035\000\030\000\034\000\031\000\033\000\
\\032\000\032\000\033\000\031\000\000\000\
\\001\000\021\000\043\000\022\000\042\000\023\000\041\000\024\000\040\000\
\\025\000\039\000\026\000\038\000\027\000\037\000\028\000\036\000\
\\029\000\035\000\030\000\034\000\031\000\033\000\032\000\032\000\
\\033\000\031\000\037\000\072\000\000\000\
\\001\000\034\000\000\000\000\000\
\\001\000\034\000\089\000\000\000\
\\001\000\038\000\082\000\000\000\
\\001\000\042\000\056\000\043\000\055\000\000\000\
\"
val actionRowNumbers =
"\030\000\027\000\031\000\046\000\
\\043\000\047\000\028\000\046\000\
\\000\000\034\000\033\000\045\000\
\\038\000\037\000\044\000\052\000\
\\001\000\029\000\001\000\001\000\
\\001\000\001\000\042\000\003\000\
\\002\000\004\000\001\000\061\000\
\\028\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\000\000\059\000\
\\040\000\005\000\039\000\057\000\
\\006\000\056\000\020\000\001\000\
\\024\000\036\000\035\000\032\000\
\\009\000\012\000\011\000\010\000\
\\008\000\007\000\019\000\018\000\
\\017\000\016\000\015\000\014\000\
\\013\000\051\000\048\000\021\000\
\\041\000\049\000\000\000\000\000\
\\053\000\054\000\060\000\025\000\
\\023\000\050\000\000\000\055\000\
\\026\000\022\000\058\000"
val gotoT =
"\
\\001\000\086\000\000\000\
\\000\000\
\\000\000\
\\002\000\005\000\003\000\004\000\000\000\
\\000\000\
\\000\000\
\\006\000\009\000\000\000\
\\002\000\011\000\003\000\004\000\000\000\
\\004\000\015\000\005\000\014\000\007\000\013\000\008\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\044\000\000\000\
\\000\000\
\\007\000\047\000\000\000\
\\007\000\048\000\000\000\
\\007\000\049\000\000\000\
\\007\000\050\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\052\000\000\000\
\\000\000\
\\006\000\055\000\000\000\
\\007\000\056\000\000\000\
\\007\000\057\000\000\000\
\\007\000\058\000\000\000\
\\007\000\059\000\000\000\
\\007\000\060\000\000\000\
\\007\000\061\000\000\000\
\\007\000\062\000\000\000\
\\007\000\063\000\000\000\
\\007\000\064\000\000\000\
\\007\000\065\000\000\000\
\\007\000\066\000\000\000\
\\007\000\067\000\000\000\
\\007\000\068\000\000\000\
\\004\000\069\000\005\000\014\000\007\000\013\000\008\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\072\000\000\000\
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
\\000\000\
\\004\000\076\000\005\000\014\000\007\000\013\000\008\000\012\000\000\000\
\\004\000\077\000\005\000\014\000\007\000\013\000\008\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\083\000\005\000\014\000\007\000\013\000\008\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 87
val numrules = 36
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
 | ID of unit ->  (string) | FF of unit ->  (bool)
 | TT of unit ->  (bool) | NUM of unit ->  (int)
 | assign of unit ->  (id*Exp) | exp of unit ->  (Exp)
 | varList of unit ->  (id list) | cmd of unit ->  (CMD)
 | cmdSeq of unit ->  (CMD list) | dec of unit ->  (DEC)
 | decSeq of unit ->  (DEC list)
 | program of unit ->  (DEC list*CMD list)
end
type svalue = MlyValue.svalue
type result = DEC list*CMD list
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
fn (T 33) => true | _ => false
val showTerminal =
fn (T 0) => "IF"
  | (T 1) => "THEN"
  | (T 2) => "ELSE"
  | (T 3) => "ENDIF"
  | (T 4) => "NUM"
  | (T 5) => "TT"
  | (T 6) => "FF"
  | (T 7) => "ID"
  | (T 8) => "PROGRAM"
  | (T 9) => "START"
  | (T 10) => "TYPEOF"
  | (T 11) => "EOS"
  | (T 12) => "COMMA"
  | (T 13) => "LSPAR"
  | (T 14) => "RSPAR"
  | (T 15) => "ASSIGN"
  | (T 16) => "NEGATE"
  | (T 17) => "LPAR"
  | (T 18) => "RPAR"
  | (T 19) => "NOT"
  | (T 20) => "AND"
  | (T 21) => "LT"
  | (T 22) => "LEQ"
  | (T 23) => "EQ"
  | (T 24) => "GT"
  | (T 25) => "GEQ"
  | (T 26) => "NEQ"
  | (T 27) => "PLUS"
  | (T 28) => "MINUS"
  | (T 29) => "TIMES"
  | (T 30) => "DIV"
  | (T 31) => "MOD"
  | (T 32) => "OR"
  | (T 33) => "EOF"
  | (T 34) => "ILLCH"
  | (T 35) => "WHILE"
  | (T 36) => "DO"
  | (T 37) => "ENDWH"
  | (T 38) => "READ"
  | (T 39) => "WRITE"
  | (T 40) => "VAR"
  | (T 41) => "INTX"
  | (T 42) => "BOOLX"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36)
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, RSPAR1right)) :: ( _, ( MlyValue.cmdSeq 
cmdSeq1, _, _)) :: _ :: ( _, ( MlyValue.decSeq decSeq1, _, _)) :: _ ::
 ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, PROGRAM1left, _)) :: 
rest671)) => let val  result = MlyValue.program (fn _ => let val  ID1
 = ID1 ()
 val  (decSeq as decSeq1) = decSeq1 ()
 val  (cmdSeq as cmdSeq1) = cmdSeq1 ()
 in (decSeq,cmdSeq)
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, RSPAR1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.decSeq decSeq1, _, decSeq1right)) :: _ :: (
 _, ( MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result
 = MlyValue.decSeq (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decSeq as decSeq1) = decSeq1 ()
 in (dec::decSeq)
end)
 in ( LrTable.NT 1, ( result, dec1left, decSeq1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.decSeq (fn _ => ([])
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( _, _, INTX1right)) :: _ :: ( _, ( MlyValue.varList 
varList1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  
result = MlyValue.dec (fn _ => let val  (varList as varList1) = 
varList1 ()
 in (INT(varList))
end)
 in ( LrTable.NT 2, ( result, VAR1left, INTX1right), rest671)
end
|  ( 4, ( ( _, ( _, _, BOOLX1right)) :: _ :: ( _, ( MlyValue.varList 
varList1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  
result = MlyValue.dec (fn _ => let val  (varList as varList1) = 
varList1 ()
 in (BOOL(varList))
end)
 in ( LrTable.NT 2, ( result, VAR1left, BOOLX1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.varList varList1, _, varList1right)) :: _ ::
 ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result
 = MlyValue.varList (fn _ => let val  (ID as ID1) = ID1 ()
 val  (varList as varList1) = varList1 ()
 in (ID::varList)
end)
 in ( LrTable.NT 5, ( result, ID1left, varList1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.varList (fn _ => let val  (ID as ID1) = 
ID1 ()
 in ([ID])
end)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.cmdSeq cmdSeq1, _, cmdSeq1right)) :: _ :: (
 _, ( MlyValue.cmd cmd1, cmd1left, _)) :: rest671)) => let val  result
 = MlyValue.cmdSeq (fn _ => let val  (cmd as cmd1) = cmd1 ()
 val  (cmdSeq as cmdSeq1) = cmdSeq1 ()
 in (cmd::cmdSeq)
end)
 in ( LrTable.NT 3, ( result, cmd1left, cmdSeq1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.cmdSeq (fn _ => ([])
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.cmd (fn _ => let val  (exp as exp1) = 
exp1 ()
 in (EXP(exp))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.assign assign1, assign1left, assign1right))
 :: rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (
assign as assign1) = assign1 ()
 in (SET(assign))
end)
 in ( LrTable.NT 4, ( result, assign1left, assign1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, 
READ1left, _)) :: rest671)) => let val  result = MlyValue.cmd (fn _ =>
 let val  (ID as ID1) = ID1 ()
 in (READ(ID))
end)
 in ( LrTable.NT 4, ( result, READ1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
WRITE1left, _)) :: rest671)) => let val  result = MlyValue.cmd (fn _
 => let val  (exp as exp1) = exp1 ()
 in (WRITE(exp))
end)
 in ( LrTable.NT 4, ( result, WRITE1left, exp1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.assign (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((ID,exp))
end)
 in ( LrTable.NT 7, ( result, ID1left, exp1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.TT TT1, TT1left, TT1right)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  (TT as TT1) = TT1 ()
 in (BoolExp(TT))
end)
 in ( LrTable.NT 6, ( result, TT1left, TT1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.FF FF1, FF1left, FF1right)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  (FF as FF1) = FF1 ()
 in (BoolExp(FF))
end)
 in ( LrTable.NT 6, ( result, FF1left, FF1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (NumExp(NUM))
end)
 in ( LrTable.NT 6, ( result, NUM1left, NUM1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 in (VarExp(ID))
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (UnExp(NOT,exp))
end)
 in ( LrTable.NT 6, ( result, NOT1left, exp1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(PLUS,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(MINUS,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(OR,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(TIMES,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(DIV,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(MOD,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(AND,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(LT,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(LEQ,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(EQ,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(GT,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(GEQ,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (BinExp(NEQ,exp1,exp2))
end)
 in ( LrTable.NT 6, ( result, exp1left, exp2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (exp as exp1) = exp1 ()
 in (UnExp(NEGATE,exp))
end)
 in ( LrTable.NT 6, ( result, NEGATE1left, exp1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.exp exp1, _, _
)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 6, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 34, ( ( _, ( _, _, ENDIF1right)) :: _ :: ( _, ( MlyValue.cmdSeq 
cmdSeq2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.cmdSeq cmdSeq1, _, _
)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, IF1left,
 _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
exp1 = exp1 ()
 val  cmdSeq1 = cmdSeq1 ()
 val  cmdSeq2 = cmdSeq2 ()
 in (ITE(exp1,cmdSeq1,cmdSeq2))
end)
 in ( LrTable.NT 6, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 35, ( ( _, ( _, _, ENDWH1right)) :: _ :: ( _, ( MlyValue.cmdSeq 
cmdSeq1, _, _)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, 
( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  (exp as exp1) = exp1 ()
 val  (cmdSeq as cmdSeq1) = cmdSeq1 ()
 in (WH(exp,cmdSeq))
end)
 in ( LrTable.NT 6, ( result, WHILE1left, ENDWH1right), rest671)
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
fun TT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.TT (fn () => i),p1,p2))
fun FF (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.FF (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun START (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPEOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun EOS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LSPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RSPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun ILLCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun INTX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
end
end
