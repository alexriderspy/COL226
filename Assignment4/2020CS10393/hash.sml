val ht : (string, int) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(10, Domain)

fun symbolTable (AST) = 
let
  fun aux1 ([],cnt) = ()
  | aux1 (INT(h)::t,cnt) = (aux1(t,aux2(h,cnt)))
  | aux1 (BOOL(h)::t,cnt) = (aux1(t,aux2(h,cnt)))

  and aux2 ([],cnt) = cnt
  | aux2 (h::t,cnt) = (HashTable.insert ht (h,cnt) ;aux2(t,cnt+1))
in
  aux1(AST,0)
end

val type_ch : (string, string) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(10, Domain)

fun typedet (AST) = 
let
  fun aux ([]) = ()
  | aux (INT(h)::t) = (auxint(h);aux(t))
  | aux (BOOL(h)::t) = (auxbool(h);aux(t))

  and auxint ([]) = ()
  | auxint (h::t) = (HashTable.insert type_ch (h,"int") ;auxint(t))

  and auxbool ([]) = ()
  | auxbool (h::t) = (HashTable.insert type_ch (h,"bool") ;auxbool(t))

in
  aux(AST)
end