signature VMC =
sig
val rules : ('a stack * int array * 'a stack) -> ('a stack * int array * 'a stack)
end

structure Vmc :> VMC =
struct
fun rules (val_st,mem,ctrl_st) = 
end