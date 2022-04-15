signature STACK =
sig
type 'a stack
exception Emptystack
exception Error of string
val create: 'a stack
val push :'a *'a stack ->'a stack
val pop :'a stack ->'a stack
val top :'a stack ->'a
val empty:'a stack -> bool
val poptop :'a stack -> ('a *'a stack) option
val nth :'a stack * int ->'a
val drop :'a stack * int ->'a stack 
val depth :'a stack -> int
val app : ('a -> unit) ->'a stack -> unit
val map : ('a -> 'b) ->'a stack -> 'b stack
val mapPartial : ('a -> 'b option) ->'a stack -> 'b stack
val find : ('a -> bool) ->'a stack ->'a option
val filter : ('a -> bool) ->'a stack ->'a stack
val foldr : ('a * 'b -> 'b) -> 'b ->'a stack -> 'b
val foldl : ('a * 'b -> 'b) -> 'b ->'a stack -> 'b
val exists : ('a -> bool) ->'a stack -> bool
val all : ('a -> bool) ->'a stack -> bool
val list2stack :'a list ->'a stack 
val stack2list:'a stack ->'a list 
val toString: ('a -> string) ->'a stack -> string 
end

structure Funstack :> STACK =
struct
type 'a stack = 'a list
exception Emptystack
exception Error of string
val create = []
fun push (x,st) = x::st
fun pop (st) = (tl(st))
fun top (st) = (hd(st))
fun empty (st) = List.null (st)
fun poptop s = 
case s of [] => NONE
| (h::t) => SOME (h,t) 
fun nth (st,i) = List.nth(st,i)
fun drop (st,i) = List.drop(st,i)
fun depth (st) = List.length(st)
fun app (x) = List.app(x)
fun map (f) = List.map (f)
fun mapPartial (f) = List.mapPartial(f)
fun find (f) = List.find(f)
fun filter (f) = List.filter(f)
fun foldr (f) = List.foldr (f)
fun foldl (f) = List.foldl (f)
fun exists (f) = List.exists (f)
fun all (f) = List.all(f)
fun list2stack lis = lis
fun stack2list st = st
fun toString (f) = 
    let
        fun aux f st = 
        let
          fun auxx (s,st)=
            case st of [] => s
            | (h::t) => auxx (s^" "^f(h),t)
        in
          auxx ("",st)
        end
    in
        aux (f)
    end 
end