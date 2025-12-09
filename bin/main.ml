open WriteLLtable.Html_cell
open WriteLLtable.Parse

let match_token = function
  | "integer" -> "TOKEN_NUMBER"
  | "+" -> "TOKEN_BINOP_ADD"
  | "-" -> "TOKEN_BINOP_SUB"
  | "*" -> "TOKEN_BINOP_MULT"
  | "/" -> "TOKEN_BINOP_DIV"
  | "%" -> "TOKEN_BINOP_MOD"
  | "||" -> "TOKEN_BINOP_OR"
  | "&amp;&amp;" -> "TOKEN_BINOP_AND"
  | "=" -> "TOKEN_BINOP_EQ"
  | "==" -> "TOKEN_BINOP_2EQUAL"
  | "!=" -> "TOKEN_BINOP_DIFF"
  | "&lt;" -> "TOKEN_BINOP_INF"
  | "&gt;" -> "TOKEN_BINOP_SUP"
  | "&lt;=" -> "TOKEN_BINOP_INFEG"
  | "&gt;=" -> "TOKEN_BINOP_SUPEG"
  | "(" -> "TOKEN_LPAR"
  | ")" -> "TOKEN_RPAR"
  | "class" -> "TOKEN_CLASS"
  | "ident" -> "TOKEN_ID"
  | "{" -> "TOKEN_LACC"
  | "}" -> "TOKEN_RACC"
  | "[" -> "TOKEN_LCRO"
  | "]" -> "TOKEN_RCRO"
  | ";" -> "TOKEN_PTSVG"
  | "," -> "TOKEN_VG"
  | "!" -> "TOKEN_OPP"
  | "." -> "TOKEN_PTS"
  | "string" -> "TOKEN_STR"
  | "$" -> "TOKEN_EOF"
  | "boolean" -> "TOKEN_BOOLEAN"
  | "else" -> "TOKEN_ELSE"
  | "extends" -> "TOKEN_EXTENDS"
  | "false" -> "TOKEN_FALSE"
  | "for" -> "TOKEN_FOR"
  | "if" -> "TOKEN_IF"
  | "instanceof" -> "TOKEN_INSTANCEOF"
  | "int" -> "TOKEN_INT"
  | "new" -> "TOKEN_NEW"
  | "null" -> "TOKEN_NULL"
  | "public" -> "TOKEN_PUBLIC"
  | "private" -> "TOKEN_PRIVATE"
  | "return" -> "TOKEN_RETURN"
  | "static" -> "TOKEN_STATIC"
  | "this" -> "TOKEN_THIS"
  | "true" -> "TOKEN_TRUE"
  | "void" -> "TOKEN_VOID"
  | "ident_type" -> "TOKEN_ID_TYPE"
  | a ->     
    Printf.eprintf "WARNING: token %s undefined!\n" a;
    (String.uppercase_ascii a)


let match_rule = function
  | "file" -> "FIL"
  | "class_list" -> "CLASS_LIST"
  | "classe" -> "CLASSE"
  | "public_opt" -> "PUBLIC_OPT"
  | "static_opt" -> "STATIC_OPT"
  | "extends_opt" -> "EXTENDS_OPT"
  | "decl_list" -> "DECL_LIST"
  | "decl_type" -> "DECL_TYPE"
  | "decl_type_ident" -> "DECL_TYPE_IDENT"
  | "decl" -> "DECLA"
  | "decl_public" -> "DECL_PUBLIC"
  | "constructeur_tail" -> "CONST_TAIL"
  | "constructeur" -> "CONST"
  | "params_opt" -> "PARAMS_OPT"
  | "params" -> "PARAMS"
  | "params2" -> "PARAMS2"
  | "type" -> "TYPE"
  | "expr_opt" -> "EXPR_OPT"
  | "expr" -> "EXPR"
  | "assignement" -> "ASSIGN"
  | "assignement_ident" -> "ASSIGN_ID"
  | "lor_expr" -> "LOR_EXPR"
  | "lor_expr_suite" -> "LOR_EXPR_SUITE"
  | "land_expr" -> "LAND_EXPR"
  | "land_expr_suite" -> "LAND_EXPR_SUITE"
  | "equality_expr" -> "EQ_EXPR"
  | "equality_expr_suite" -> "EQ_EXPR_SUITE"
  | "relational_expr" -> "RELATIONAL_EXPR"
  | "relational_expr_suite" -> "RELATIONAL_EXPR_SUITE"
  | "add" -> "ADD"
  | "add_suite" -> "ADD_SUITE"
  | "mult_expr" -> "MULT"
  | "mult_suite" -> "MULT_SUITE"
  | "unary" -> "UNARY"
  | "cast_or_postfix" -> "C_O_P"
  | "cast_or_postfix_par" -> "C_O_P_PAR"
  | "postfix_expr" -> "POSTFIX_EXPR"
  | "postfix_tail" -> "POSTFIX_TAIL"
  | "primary" -> "PRIMARY"
  | "primary_par" -> "PRIMARY_PAR"
  | "lexpr_opt" -> "LEXPR_OPT"
  | "lexpr" -> "LEXPR"
  | "lexpr_expr" -> "LEXPR_EXPR"
  | "stmt_list" -> "STMT_LIST"
  | "stmt" -> "STMT"
  | "stmt_type_ident" -> "STMT_TYPE_IDENT"
  | "stmt_if" -> "STMT_IF"
  | "block" -> "BLOCK"
  | "methode" -> "METHODE"
  | "const_or_meth" -> "CONST_OR_METH"
  | "cro_opt" -> "CRO_OPT"
  | a ->
    Printf.eprintf "WARNING: rule %s undefined!\n" a;
    (String.uppercase_ascii a)

let wrap_around = 
  Printf.sprintf 
      "#include \"prod_table.h\"\n\
        \n\
        void init_prod_table(void)\n\
        {\
        %s\n\
        }"

let () =
  let html = Sys.argv.(1) in
  let input =
    let ic = open_in html in
    let len = in_channel_length ic in
    let buf = really_input_string ic len in
    close_in ic;
    buf
  in

  let result =
    parser htmltable input 
    |> generate_all_c_productions match_token match_rule
  in

  match result with
  | Ok c_code -> print_endline (wrap_around c_code)
  | Error e -> Printf.eprintf "Error: %s\n" e
