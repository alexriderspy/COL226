(* compiler.sml *)
structure Pi :
sig val compile : string -> DataTypes.Pi
val convertToPostfix : DataTypes.Pi -> unit
end =
struct
exception PiError;
fun convertToPostfix (a) = 
    case a of Pi(y) => (print("ok\n"))
fun compile (fileName) =
    let val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
            n => if TextIO.endOfStream inStream
                then ""
                else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line,col) =>
            print (fileName^"["^Int.toString line^":"
                ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = PiParser.parse
                        (15,
                        (PiParser.makeLexer grab fileName),
                        printError,
                        fileName)
            handle PiParser.ParseError => raise PiError;    
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in (convertToPostfix(tree);tree)
    end
end;
