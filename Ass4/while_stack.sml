signature STACK =
sig
type 'a Stack
exception EmptyStack
exception Error of string
val create: unit ->'a Stack
val push :'a *'a Stack ->'a Stack
val pop :'a Stack ->'a Stack
val top :'a Stack ->'a
val empty:'a Stack -> bool
val poptop :'a Stack -> ('a *'a Stack) option
val nth :'a Stack * int ->'a
val drop :'a Stack * int ->'a Stack
val depth :'a Stack -> int
val app : ('a -> unit) ->'a Stack -> unit
val map : ('a -> 'b) ->'a Stack -> 'b Stack
val mapPartial : ('a -> 'b option) ->'a Stack -> 'b Stack
val find : ('a -> bool) ->'a Stack ->'a option
val filter : ('a -> bool) ->'a Stack ->'a Stack
val foldr : ('a * 'b -> 'b) -> 'b ->'a Stack -> 'b
val foldl : ('a * 'b -> 'b) -> 'b ->'a Stack -> 'b
val exists : ('a -> bool) ->'a Stack -> bool
val all : ('a -> bool) ->'a Stack -> bool
val list2Stack :'a list ->'a Stack 
val stack2list:'a Stack ->'a list 
val toString: ('a -> string) ->'a Stack -> string
end

structure FunStack :> STACK =
struct
type 'a Stack = 'a list
exception EmptyStack
exception Error of string

fun push x st = x::st 
fun pop x st = tl st
fun top st = hd st
fun empty [] = true
|   h :: rest = false
fun poptop [] = NONE
| h :: rest = SOME 
fun nth st i = List.nth(st,i)
fun drop st i = List.drop(st,i)
fun depth st = List.length(st)
fun app st unit = List.app(st unit)
fun map f st = List.map (f, st)
fun mapPartial 
fun find
fun filter
fun foldr
fun foldl
fun exists
fun all
fun list2Stack lis = lis
fun stack2list st = st
fun toString st = List.toString(st)
end