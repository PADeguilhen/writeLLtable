open Parse

type prod_item = Terminal of string | NonTerminal of string
type production = { lhs : string; rhs : prod_item list }
type ll1_table = { first : prod_item; rule : production }

let ws = take_while is_whitespace

let tag name body =
  (string ("<" ^ name) <?> "opening <" ^ name ^ "> tag")
  *> (take_while (( <> ) '>') <?> "tag attributes")
  *> (string ">" <?> "end of opening tag")
  *> ws *> body (* body of tag *)
  <* ws
  <* (string ("</" ^ name) <?> "closing </" ^ name ^ "> tag")
  <* (take_while (( <> ) '>') <?> "closing tag attributes")
  <* (string ">" <?> "end of closing tag")

(* Special characters (Tokens & Non terminals) *)
let underline body = tag "u" body <?> "underline tag <u>"
let italic body = tag "i" body <?> "italic tag <i>"
let bold body = tag "b" body <?> "bold tag <b>"
let thead body = tag "thead" body <?> "table head <thead>"
let tbody body = tag "tbody" body <?> "table body <tbody>"
let tr body = tag "tr" body <?> "table row <tr>"
let th body = tag "th" body <?> "table header cell <th>"
let td body = tag "td" body <?> "table data cell <td>"
let li body = tag "li" body <?> "list item <li>"
let ul body = tag "ul" body <?> "unordered list <ul>"
let colgroup body = tag "colgroup" body <?> "column group <colgroup>"
let table body = tag "table" body <?> "table <table>"

let headCell body =
  bold body <?> "bold header cell"
  <|> (underline body <?> "underlined header cell")
  <|> return ""

let tableCell body =
  bold body
  >>| (fun a -> Terminal a)
  <?> "terminal (bold text)"
  <|> (italic body >>| (fun a -> NonTerminal a) <?> "non-terminal (italic text)")
  <|> (underline body
      >>| (fun a -> Terminal a)
      <?> "epsilon or special terminal (underlined text)")

let token = take_while (( <> ) '<') <?> "token text"

let singleProd =
  li
  @@ lift3
       (fun a _ c -> { lhs = a; rhs = c })
       (ws *> italic token <* ws <?> "left-hand side (italic non-terminal)")
       (ws *> string "→" <* ws <?> "arrow symbol →")
       (many (ws *> tableCell token) <?> "right-hand side production items")
  <?> "production rule in <li>"

let tableProd =
  ul @@ (singleProd >>| (fun p -> Some p) <|> return None)
  <?> "table cell production (may be empty)"

let tableRow =
  tr
  @@ lift2
       (fun _ b -> b)
       (th @@ italic @@ token <?> "row header (non-terminal name)")
       (many (td @@ tableProd) <?> "table data cells with productions")
  <?> "table row"

(* list of firsts *)
let tableHead =
  thead @@ tr @@ many @@ th @@ (headCell token >>| fun t -> Terminal t)
  <?> "table header row"

let tableBody = tbody @@ many tableRow <?> "table body with production rows"

let htmltable =
  table
  @@ lift2
       (fun a b -> (a, b))
       (tableHead <?> "parsing table header")
       (tableBody <?> "parsing table body")
  <?> "LL(1) parsing table"

let generate_c_production prod terminal match_tokens match_rules =
  let items =
    List.map
      (function
        | Terminal "ε" -> Printf.sprintf "{}"
        | Terminal t ->
            Printf.sprintf "{.token = %s, .type = TOKEN}" (match_tokens t)
        | NonTerminal nt ->
            Printf.sprintf "{.rule = %s, .type = RULE}" (match_rules nt))
      prod.rhs
  in
  let items_str = String.concat ", " items in
  let count = List.length prod.rhs in
  let lhs_c = match_rules prod.lhs in
  let terminal_c = match_tokens terminal in
  Printf.sprintf
    "    prod_table[%s][%s] = (Production) {.exp = {.items = {%s}, .count = \
     %d}, .action = action_stub};"
    lhs_c terminal_c items_str count

let generate_all_c_productions match_tokens match_rules parsed_table =
  match parsed_table with
  | Error e -> Error e
  | Ok (header, rows) ->
      let header = List.tl header in
      let productions =
        List.concat_map
          (fun row ->
            List.mapi
              (fun col_idx prod_opt ->
                match prod_opt with
                | None -> None
                | Some prod ->
                    (* Get the terminal from the header at this column index *)
                    if col_idx < List.length header then
                      let terminal = List.nth header col_idx in
                      let terminal_str =
                        match terminal with
                        | Terminal s -> s
                        | NonTerminal s -> s
                      in
                      Some
                        (generate_c_production prod terminal_str match_tokens
                           match_rules)
                    else None)
              row)
          rows
      in
      let filtered_productions = List.filter_map (fun x -> x) productions in
      Ok (String.concat "\n" filtered_productions)
