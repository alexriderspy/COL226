%%
%pos int*int
%term program|::|var|:|;|int|bool|,|{|}|:=|read|write|if|then|else|endif|while|do|endwh|(|)|~||||&&|tt|ff|!|<|<=|=|>|>=|<>|+|-|*|/|%|a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z|A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z|0|1|2|3|4|5|6|7|8|9
%nonterm Program|Identifier|Block|DeclarationSeq|CommandSeq|Declaration|VariableList|Type|Variable|Command|Expression|IntExpression|BoolExpression|AddOp|IntTerm|MultOp|IntFactor|Numeral|BoolTerm|BoolFactor|RelOp|Letter|Digit
%eop EOF
%start Program
%%


