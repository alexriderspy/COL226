(* datatypes.sml *)
signature DATATYPES =
    sig datatype PROG = PROG of string * BLK
            and BLK = BLK of string list * string list
            and DEC = DEC of string list
            and CMD = CMD of string
            and SET = SET of string * string
            and ITE = ITE of BEXP * string list * string list
            and WH = WH of BEXP * string list
            and IEXP = IEXP of string
            and BEXP = BEXP of string
            and PLUS = PLUS of IEXP * IEXP
            and MINUS = MINUS of IEXP * IEXP
            and TIMES = TIMES of IEXP * IEXP
            and DIV = DIV of IEXP * IEXP
            and MOD = MOD of IEXP * IEXP
            and LT = LT of IEXP * IEXP
            and LEQ = LEQ of IEXP * IEXP
            and EQ = EQ of IEXP * IEXP
            and GT = GT of IEXP * IEXP
            and GEQ = GEQ of IEXP * IEXP
            and NEQ = NEQ of IEXP * IEXP
            and NOT = NOT of IEXP
            and UMINUS = UMINUS of IEXP
    end;

structure DataTypes : DATATYPES =
    struct
        datatype PROG = PROG of string * BLK
            and BLK = BLK of string list * string list
            and DEC = DEC of string list
            and CMD = CMD of string
            and SET = SET of string * string
            and ITE = ITE of BEXP * string list * string list
            and WH = WH of BEXP * string list
            and IEXP = IEXP of string
            and BEXP = BEXP of string
            and PLUS = PLUS of IEXP * IEXP
            and MINUS = MINUS of IEXP * IEXP
            and TIMES = TIMES of IEXP * IEXP
            and DIV = DIV of IEXP * IEXP
            and MOD = MOD of IEXP * IEXP
            and LT = LT of IEXP * IEXP
            and LEQ = LEQ of IEXP * IEXP
            and EQ = EQ of IEXP * IEXP
            and GT = GT of IEXP * IEXP
            and GEQ = GEQ of IEXP * IEXP
            and NEQ = NEQ of IEXP * IEXP
            and NOT = NOT of IEXP
            and UMINUS = UMINUS of IEXP
    end;