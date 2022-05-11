signature VMC =
sig
val rules : (string Funstack.stack * int array * string Funstack.stack ) -> (string Funstack.stack * int array * string Funstack.stack)
end

structure Vmc :> VMC =
struct
fun iden (x) = x;
fun rules (V,M,C) =
    let
    fun rules_aux(V,M,C,l) = 
    if Funstack.empty(C) = true then (V,M,C,l)
    else
    let
        val top=Funstack.top(C)
        val str = ("Value Stack: "^(Funstack.toString iden V)^"\n"^"Control Stack: "^(Funstack.toString iden C)^"\n\n");
        in
        (*This will print the value of control stack and value stack at each stage*)
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
                    else if (Int.fromString(top1) = NONE) then Array.sub(M,HashTable.lookup ht (top1))
                    else valOf (Int.fromString(top1))
                  end);
                  rules_aux(V2,M,Funstack.pop(C),[top]@l)
              )
            end
            | "READ" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
            in
              (
                  print("\nInput: ");
                  Array.update(M,HashTable.lookup ht (top1),
                  let
                    val SOME x = TextIO.inputLine(TextIO.stdIn)
                  in
                    if (x = "true") then 1
                    else if (x = "false") then 0
                    else valOf(Int.fromString(x))
                  end);
                  rules_aux(V1,M,Funstack.pop(C),[top]@l)
              )
            end
            | "WRITE" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
            in
              (
                if Int.fromString(top1) = NONE andalso Bool.fromString(top1) = NONE then print("Output: "^Int.toString(Array.sub(M,HashTable.lookup ht (top1)))^"\n\n")
                else if Int.fromString(top1) = NONE then print("\nOutput: "^top1^"\n\n")
                else print("\nOutput: "^top1^"\n\n");
                rules_aux(V1,M,Funstack.pop(C),[top]@l)
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
                  rules_aux(Funstack.push(Int.toString(x+y),V2),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Int.toString(y-x),V2),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Int.toString(x*y),V2),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Int.toString(y div x),V2),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Int.toString(y mod x),V2),M,Funstack.pop(C),[top]@l)

              )
            end
        |   "NEGATE" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val x = if Int.fromString(top1) = NONE then Array.sub(M,HashTable.lookup ht (top1)) else valOf(Int.fromString(top1))
            in
              (
                  rules_aux(Funstack.push(Int.toString(~x),V1),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Bool.toString(y andalso x),V2),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Bool.toString(y orelse x),V2),M,Funstack.pop(C),[top]@l)

              )
            end
        |   "NOT" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val x = if Bool.fromString(top1) = NONE then (if Array.sub(M,HashTable.lookup ht (top1)) = 1 then true else false) else (if top1 = "true" then true else false)
            in
              (
                  rules_aux(Funstack.push(Bool.toString(not x),V1),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Bool.toString(y < x),V2),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Bool.toString(y <= x),V2),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Bool.toString(y > x),V2),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Bool.toString(y >= x),V2),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Bool.toString(y = x),V2),M,Funstack.pop(C),[top]@l)

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
                  rules_aux(Funstack.push(Bool.toString(y <> x),V2),M,Funstack.pop(C),[top]@l)

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
                    val c1 = rules_aux(V1,M,Funstack.pop(C),l)
                    val c2 = remove_1st(#1 c1, #2 c1, Funstack.pop(#3 c1),l)
                  in
                    (rules_aux(#1 c2,#2 c2,#3 c2,#4 c2))
                  end
                  else  
                  let
                    val c2 = (rules_aux(remove_1st(V1,M,Funstack.pop(C),l)))
                  in
                    (rules_aux(#1 c2,#2 c2, Funstack.pop(#3 c2),#4 c2 )) 
                  end
              )
            end
        |   "CMDSEQ" => (V,M,Funstack.pop(C),[top]@l)
        |   "ITE" => (rules_aux(V,M,Funstack.pop(C),[top]@l))
        |   "CMD" => (rules_aux(V,M,Funstack.pop(C),[top]@l))
        |   "WH_B" => 
            let
              val SOME (top1,V1) = Funstack.poptop(V)
              val x = if Bool.fromString(top1) = NONE then (if Array.sub(M,HashTable.lookup ht (top1)) = 1 then true else false) else (if top1 = "true" then true else false)
            in
              (
                  if (x = true) then 
                  let
                    val c1 = rules_aux(V1,M,Funstack.pop(C),[top]@l)
                    val c2 = push_wh(#3 c1,#4 c1)
                  in
                    (rules_aux(#1 c1,#2 c1,c2, #4 c1))
                  end
                  else  
                  let
                    val c2 = (rules_aux(remove_1st(V1,M,Funstack.pop(C),l)))
                  in
                    (rules_aux(#1 c2,#2 c2, #3 c2,l)) 
                  end
              )
            end
        |   "WH" => rules_aux(V,M,Funstack.pop(C),[top]@l)
        |   c => rules_aux(Funstack.push(c,V),M,Funstack.pop(C),[top]@l)
        handle Empty => (print("Empty Exception!\n");(V,M,C,[]))            
        end

    and remove_1st(V,M,C,l) =
    let
      
    in
      if (Funstack.top(C) = "CMDSEQ") then (V,M,Funstack.pop(C),l)
      else remove_1st(V,M,Funstack.pop(C),l)
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

    val f = rules_aux(V,M,C,[]);
    in (
      (#1 f,#2 f,#3 f)
    )
    end
    
end