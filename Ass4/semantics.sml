signature VMC =
sig
val rules : (string Funstack.stack * int array * string Funstack.stack ) -> (string Funstack.stack * int array * string Funstack.stack)
end

structure Vmc :> VMC =
struct

fun iden (x) = x;
fun rules (V,M,C) =
    let
    fun rules_aux(V,M,C,l,i) = 
    if Funstack.empty(C) = true then (V,M,C,l,i)
    else
    let
        val top=Funstack.top(C)
        val str = ((Funstack.toString iden V)^"\n"^(Funstack.toString iden C)^"\n"^((Funstack.toString iden (Funstack.list2stack (l)))^"\n"));
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
                  rules_aux(V2,M,Funstack.pop(C),[top]@l,i+1)
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
                  rules_aux(Funstack.push(Int.toString(x+y),V2),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Int.toString(y-x),V2),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Int.toString(x*y),V2),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Int.toString(y div x),V2),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Int.toString(y mod x),V2),M,Funstack.pop(C),[top]@l,i+1)

              )
            end
        |   "NEGATE" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val x = if Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else valOf(Int.fromString(top1))
            in
              (
                  rules_aux(Funstack.push(Int.toString(~x),V1),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Bool.toString(y andalso x),V2),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Bool.toString(y orelse x),V2),M,Funstack.pop(C),[top]@l,i+1)

              )
            end
        |   "NOT" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val x = if Bool.fromString(top1) = NONE then (if Array.sub(M,HashTable.lookup ht (top1)) = 1 then true else false) else (if top1 = "true" then true else false)
            in
              (
                  rules_aux(Funstack.push(Bool.toString(not x),V1),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Bool.toString(y < x),V2),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Bool.toString(y <= x),V2),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Bool.toString(y > x),V2),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Bool.toString(y >= x),V2),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Bool.toString(y = x),V2),M,Funstack.pop(C),[top]@l,i+1)

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
                  rules_aux(Funstack.push(Bool.toString(y <> x),V2),M,Funstack.pop(C),[top]@l,i+1)

              )
            end
        |   "ITE_B" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val x = if Bool.fromString(top1) = NONE then (if Array.sub(M,HashTable.lookup ht (top1)) = 1 then true else false) else (if top1 = "true" then true else false)
              
              
            in
              (
                  if (x = true) then 
                  let
                    val c1 = rules_aux(V1,M,Funstack.pop(C),l,i+1)
                    val c2 = remove_1st(#1 c1, #2 c1, Funstack.pop(#3 c1),l,i+1)
                  in
                    (rules_aux(#1 c2,#2 c2,#3 c2,#4 c2,(#5 c2) + 1))
                  end
                  else  
                  let
                    val c2 = (rules_aux(remove_1st(V1,M,Funstack.pop(C),l,i+1)))
                  in
                    (rules_aux(#1 c2,#2 c2, Funstack.pop(#3 c2),#4 c2, (#5 c2)+1 )) 
                  end
              )
            end
        |   "CMDSEQ" => (V,M,Funstack.pop(C),[top]@l,i+1)
        |   "ITE" => (rules_aux(V,M,Funstack.pop(C),[top]@l,i+1))
        |   "CMD" => (rules_aux(V,M,Funstack.pop(C),[top]@l,i+1))
        |   "WH_B" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val x = if Bool.fromString(top1) = NONE then (if Array.sub(M,HashTable.lookup ht (top1)) = 1 then true else false) else (if top1 = "true" then true else false)
            in
              (
                  if (x = true) then 
                  let
                    val c1 = rules_aux(V1,M,Funstack.pop(C),[top]@l,i+1)
                    val c2 = push_wh(#3 c1,#4 c1)
                  in
                    (rules_aux(#1 c1,#2 c1,c2, #4 c1, (#5 c1+1)))
                  end
                  else  
                  let
                    val c2 = (rules_aux(remove_1st(V1,M,Funstack.pop(C),l,i+1)))
                  in
                    (rules_aux(#1 c2,#2 c2, #3 c2,l,i+1)) 
                  end
              )
            end
        |   "WH" => rules_aux(V,M,Funstack.pop(C),[top]@l,i+1)
        |   c => rules_aux(Funstack.push(c,V),M,Funstack.pop(C),[top]@l,i+1)
        (* handle Empty => (print("Empty Exception!\n");(V,M,C,[],i+1))             *)
        end

    and remove_1st(V,M,C,l,i) =
    let
      
    in
      if (Funstack.top(C) = "CMDSEQ") then (V,M,Funstack.pop(C),l,i+1)
      else remove_1st(V,M,Funstack.pop(C),l,i+1)
    end
    
    and push_wh (C,l) = 
    let
      
    in
      if hd(l) = "WH_B" then push_b(Funstack.push(hd(l),C),tl(l)) else push_wh(Funstack.push(hd(l),C),tl(l))
    end

    and push_b(C,l) = 
    let
      
    in
      if(l=[] orelse hd(l) = "CMD" orelse hd(l) = "CMDSEQ") then C else push_b(Funstack.push(hd(l),C),tl(l))
    end

    val f = rules_aux(V,M,C,[],0);
    in (
      (#1 f,#2 f,#3 f)
    )
    end
    
end