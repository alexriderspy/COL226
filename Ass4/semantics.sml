signature VMC =
sig
val rules : (string Funstack.stack * int array * string Funstack.stack) -> (string Funstack.stack * int array * string Funstack.stack)
end

structure Vmc :> VMC =
struct
fun iden (x) = x;
fun rules (V,M,C) = 
    if Funstack.empty(C) = true then (V,M,C)
    else
        let
            val top=Funstack.top(C)
            val str = ((Funstack.toString iden V)^"\n"^(Funstack.toString iden C)^"\n");
        in
        print(str);
        case top of 
            "SET" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
            in
              (
                  Array.update(M,HashTable.lookup ht (top2),
                  let
                    
                  in
                    if (top1 = "true") then 1
                    else if (top1 = "false") then 0
                    else valOf (Int.fromString(top1))
                  end);
                  rules(V2,M,Funstack.pop(C))
              )
            end
        |   "PLUS" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else valOf(Int.fromString(top1))
              val y = if Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else valOf(Int.fromString(top2))
            in
              (
                  rules(Funstack.push(Int.toString(x+y),V2),M,Funstack.pop(C))

              )
            end
        |   "MINUS" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else valOf(Int.fromString(top1))
              val y = if Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else valOf(Int.fromString(top2))
            in
              (
                  rules(Funstack.push(Int.toString(y-x),V2),M,Funstack.pop(C))

              )
            end
        |   "TIMES" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else valOf(Int.fromString(top1))
              val y = if Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else valOf(Int.fromString(top2))
            in
              (
                  rules(Funstack.push(Int.toString(x*y),V2),M,Funstack.pop(C))

              )
            end
        |   "DIV" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else valOf(Int.fromString(top1))
              val y = if Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else valOf(Int.fromString(top2))
            in
              (
                  rules(Funstack.push(Int.toString(y div x),V2),M,Funstack.pop(C))

              )
            end
        |   "MOD" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else valOf(Int.fromString(top1))
              val y = if Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else valOf(Int.fromString(top2))
            in
              (
                  rules(Funstack.push(Int.toString(y mod x),V2),M,Funstack.pop(C))

              )
            end
        |   "NEGATE" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val x = if Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else valOf(Int.fromString(top1))
            in
              (
                  rules(Funstack.push(Int.toString(~x),V1),M,Funstack.pop(C))

              )
            end
        |   "AND" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Bool.fromString(top1) = NONE then (if Array.sub(M,HashTable.lookup ht (top1)) = 1 then true else false) else (if top1 = "true" then true else false)
              val y = if Bool.fromString(top2) = NONE then (if Array.sub(M,HashTable.lookup ht (top2)) = 1 then true else false) else (if top2 = "true" then true else false)
            in
              (
                  rules(Funstack.push(Bool.toString(y andalso x),V2),M,Funstack.pop(C))

              )
            end
        |   "OR" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Bool.fromString(top1) = NONE then (if Array.sub(M,HashTable.lookup ht (top1)) = 1 then true else false) else (if top1 = "true" then true else false)
              val y = if Bool.fromString(top2) = NONE then (if Array.sub(M,HashTable.lookup ht (top2)) = 1 then true else false) else (if top2 = "true" then true else false)
            in
              (
                  rules(Funstack.push(Bool.toString(y orelse x),V2),M,Funstack.pop(C))

              )
            end
        |   "NOT" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val x = if Bool.fromString(top1) = NONE then (if Array.sub(M,HashTable.lookup ht (top1)) = 1 then true else false) else (if top1 = "true" then true else false)
            in
              (
                  rules(Funstack.push(Bool.toString(not x),V1),M,Funstack.pop(C))

              )
            end
        |   "LT" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Bool.fromString(top1) = NONE andalso Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else if Bool.fromString(top1) = NONE then valOf(Int.fromString(top1)) else (
                    if(top1 = "true") then 1
                    else 0
              )
              val y = if Bool.fromString(top2) = NONE andalso Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else if Bool.fromString(top2) = NONE then valOf(Int.fromString(top2)) else (
                    if(top2 = "true") then 1
                    else 0
              )
            in 
              (
                  rules(Funstack.push(Bool.toString(y < x),V2),M,Funstack.pop(C))

              )
            end
        |   "LEQ" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Bool.fromString(top1) = NONE andalso Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else if Bool.fromString(top1) = NONE then valOf(Int.fromString(top1)) else (
                    if(top1 = "true") then 1
                    else 0
              )
              val y = if Bool.fromString(top2) = NONE andalso Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else if Bool.fromString(top2) = NONE then valOf(Int.fromString(top2)) else (
                    if(top2 = "true") then 1
                    else 0
              )
            in 
              (
                  rules(Funstack.push(Bool.toString(y <= x),V2),M,Funstack.pop(C))

              )
            end
        |   "GT" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Bool.fromString(top1) = NONE andalso Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else if Bool.fromString(top1) = NONE then valOf(Int.fromString(top1)) else (
                    if(top1 = "true") then 1
                    else 0
              )
              val y = if Bool.fromString(top2) = NONE andalso Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else if Bool.fromString(top2) = NONE then valOf(Int.fromString(top2)) else (
                    if(top2 = "true") then 1
                    else 0
              )
            in 
              (
                  rules(Funstack.push(Bool.toString(y > x),V2),M,Funstack.pop(C))

              )
            end
        |   "GEQ" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Bool.fromString(top1) = NONE andalso Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else if Bool.fromString(top1) = NONE then valOf(Int.fromString(top1)) else (
                    if(top1 = "true") then 1
                    else 0
              )
              val y = if Bool.fromString(top2) = NONE andalso Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else if Bool.fromString(top2) = NONE then valOf(Int.fromString(top2)) else (
                    if(top2 = "true") then 1
                    else 0
              )
            in 
              (
                  rules(Funstack.push(Bool.toString(y >= x),V2),M,Funstack.pop(C))

              )
            end
        |   "EQ" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Bool.fromString(top1) = NONE andalso Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else if Bool.fromString(top1) = NONE then valOf(Int.fromString(top1)) else (
                    if(top1 = "true") then 1
                    else 0
              )
              val y = if Bool.fromString(top2) = NONE andalso Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else if Bool.fromString(top2) = NONE then valOf(Int.fromString(top2)) else (
                    if(top2 = "true") then 1
                    else 0
              )
            in 
              (
                  rules(Funstack.push(Bool.toString(y = x),V2),M,Funstack.pop(C))

              )
            end
        |   "NEQ" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val SOME (top2,V2) = Funstack.poptop(V1)
              val x = if Bool.fromString(top1) = NONE andalso Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else if Bool.fromString(top1) = NONE then valOf(Int.fromString(top1)) else (
                    if(top1 = "true") then 1
                    else 0
              )
              val y = if Bool.fromString(top2) = NONE andalso Int.fromString(top2) = NONE then Array.sub(M,HashTable.lookup ht (top2)) else if Bool.fromString(top2) = NONE then valOf(Int.fromString(top2)) else (
                    if(top2 = "true") then 1
                    else 0
              )
            in 
              (
                  rules(Funstack.push(Bool.toString(y <> x),V2),M,Funstack.pop(C))

              )
            end
        |   "ITE_B" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val x = if Bool.fromString(top1) = NONE then (if Array.sub(M,HashTable.lookup ht (top1)) = 1 then true else false) else (if top1 = "true" then true else false)
              val c1 = rules(V1,M,C)
              val c2 = (rules(remove_1st(V1,M,C)))
            in
              (
                  if (x = true) then (rules(#1 c1,#2 c1,Funstack.pop(#3 c1)))
                  else  (rules(#1 c2,#2 c2, Funstack.pop(#3 c2))) 

              )
            end
        |   "CMDSEQ" => (V,M,C)
        |   c => rules(Funstack.push(c,V),M,Funstack.pop(C))            
        end
    and remove_1st(V,M,C) =
    let
      
    in
      if (Funstack.top(C) = "CMDSEQ") then (V,M,Funstack.pop(C))
      else remove_1st(V,M,Funstack.pop(C))
    end
    
end