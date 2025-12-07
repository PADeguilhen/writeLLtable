module State = struct
  type 'a t =
    | Done of int * 'a
    | Fail of int * string list
    | Lazy of 'a t Lazy.t
end

module Input = struct
  (* (input, pos, is_increasing, length) *)
  type t = {
    input : string;
    mutable pos : int;
    mutable is_increasing : bool;
    length : int;
  }

  let input_of_string ?(is_increasing = true) str =
    let len = String.length str in
    {
      input = str;
      pos = (if is_increasing then 0 else len - 1);
      is_increasing;
      length = len;
    }

  let pos v = v.pos

  (* Can you go to the dist next element in the input (exclusive) *)
  let can_get ~dist (v : t) =
    if v.is_increasing then v.pos + dist < v.length else v.pos - dist >= 0

  let can_get_substr ~dist (v : t) =
    if v.is_increasing then v.pos + dist <= v.length else v.pos - dist + 1 >= 0

  let collect (v : t) len =
    if v.is_increasing then String.sub v.input v.pos len
    else String.sub v.input (v.pos - len + 1) len

  let get ~dist (v : t) =
    if can_get ~dist v then
      if v.is_increasing then v.input.[v.pos + dist] else v.input.[v.pos - dist]
    else
      failwith
        (Printf.sprintf "Input backend error: out of bounds in get: %d" v.pos)

  let next ~dist v =
    if v.is_increasing then v.pos <- v.pos + dist else v.pos <- v.pos - dist;
    v
end

type 'a with_input = Input.t -> 'a

(*
success takes an input string, input position, returns an ast
failure takes the same but returns an error message
*)
type ('a, 'r) success = ('a -> 'r State.t) with_input
type 'r failure = (string list -> 'r State.t) with_input

type 'a t = {
  run : 'r. (('a, 'r) success -> 'r failure -> 'r State.t) with_input;
}

let fail_k (inp : Input.t) msg = State.Fail (inp.pos, msg)
let succ_k (inp : Input.t) a = State.Done (inp.pos, a)

let rec from_state s =
  match s with
  | State.Done (pos, ast) -> State.Done (pos, ast)
  | State.Fail (pos, msg) -> State.Fail (pos, msg)
  | State.Lazy x -> from_state (Lazy.force x)

let parse ?(incr = true) p input =
  from_state
    (p.run (Input.input_of_string ~is_increasing:incr input) succ_k fail_k)

let string_of_err err =
  List.fold_left (fun acc n -> Printf.sprintf "%s %s" acc n) "" err

let parser ?(incr = true) p str =
  match parse ~incr p str with
  | Done (_pos, e) -> Ok e
  | Fail (pos, err) ->
      Error (Printf.sprintf "Error:%s at position:%d" (string_of_err err) pos)
  | _ -> Error "wtf ??"

let return v = { run = (fun input succ _fail -> succ input v) }
let fail msg = { run = (fun input _succ fail -> fail input msg) }

let ( >>= ) p f =
  {
    run =
      (fun inp succ fail ->
        let succ' inp' v = (f v).run inp' succ fail in
        p.run inp succ' fail);
  }

let ( >>| ) p f =
  {
    run =
      (fun inp succ fail ->
        let succ' inp' v = succ inp' (f v) in
        p.run inp succ' fail);
  }

let ( <$> ) f m = m >>| f
let ( <*> ) f m = f >>= fun f -> m >>| f
let lift = ( >>| )
let lift2 f m1 m2 = f <$> m1 <*> m2
let lift3 f m1 m2 m3 = f <$> m1 <*> m2 <*> m3
let lift4 f m1 m2 m3 m4 = f <$> m1 <*> m2 <*> m3 <*> m4

let ( *> ) a b =
  {
    run =
      (fun inp succ fail ->
        let succ' inp' _v = b.run inp' succ fail in
        a.run inp succ' fail);
  }

let ( <* ) a b =
  {
    run =
      (fun inp succ fail ->
        let succ0 inp0 v =
          let succ1 inp1 _ = succ inp1 v in
          b.run inp0 succ1 fail
        in
        a.run inp succ0 fail);
  }

let ( <?> ) p msg =
  {
    run =
      (fun input success failure ->
        let failure' inp' msg' = failure inp' (msg :: msg') in
        p.run input success failure');
  }

let ( <|> ) p q =
  {
    run =
      (fun input success failure ->
        let failure' inp' _msg = q.run inp' success failure in
        p.run input success failure');
  }

let advance n =
  {
    run = (fun input success _failure -> success (Input.next ~dist:n input) ());
  }

let take_while f =
  {
    run =
      (fun input success _fail ->
        let rec range l =
          if Input.can_get ~dist:l input && f (Input.get ~dist:l input) then
            range (l + 1)
          else l
        in
        let dist = range 0 in
        let result = Input.collect input dist in
        success (Input.next ~dist input) result);
  }

let take_while1 f =
  {
    run =
      (fun input success failure ->
        let rec range l =
          if Input.can_get ~dist:l input && f (Input.get ~dist:l input) then
            range (l + 1)
          else l
        in
        let len = range 0 in
        if len = 0 then failure input []
        else
          let result = Input.collect input len in
          success (Input.next ~dist:len input) result);
  }

let peek_char =
  {
    run =
      (fun input success _fail ->
        if Input.can_get ~dist:0 input then
          success input (Some (Input.get ~dist:0 input))
        else success input None);
  }

let peek_string n =
  {
    run =
      (fun input success failure ->
        if Input.can_get ~dist:n input then
          let col = Input.collect input n in
          success input col
        else failure input []);
  }

let char c =
  {
    run =
      (fun input success failure ->
        if not (Input.can_get ~dist:0 input) then failure input []
        else if Input.get ~dist:0 input = c then
          success (Input.next ~dist:1 input) c
        else failure input []);
  }

let string str =
  {
    run =
      (fun input success failure ->
        let len = String.length str in
        if Input.can_get_substr ~dist:len input then
          let res = Input.collect input len in
          if res = str then success (Input.next ~dist:len input) res
          else failure input []
        else failure input []);
  }

let cons x xs = x :: xs

let fix_lazy ~max_steps f =
  let steps = ref max_steps in
  let rec p = lazy (f r)
  and r =
    {
      run =
        (fun inp succ fail ->
          decr steps;
          if !steps < 0 then (
            steps := max_steps;
            State.Lazy (lazy ((Lazy.force p).run inp succ fail)))
          else (Lazy.force p).run inp succ fail);
    }
  in
  r

let notset = { run = (fun _buf _succ _fail -> failwith "oopsy :/") }

let fix_direct f =
  let rec p = ref notset
  and r = { run = (fun input succ fail -> !p.run input succ fail) } in
  p := f r;
  r

let fix =
  match Sys.backend_type with
  | Native -> fix_direct
  | Bytecode -> fix_direct
  | Other _ -> fun f -> fix_lazy ~max_steps:20 f

let sep_by1 s p = fix (fun m -> lift2 cons p (s *> m <|> return []))
let sep_by s p = lift2 cons p (s *> sep_by1 s p <|> return []) <|> return []
let pos = { run = (fun input succ _fail -> succ input (Input.pos input)) }
let many p = fix (fun m -> lift2 cons p m <|> return [])
let is_integer = function '0' .. '9' -> true | _ -> false

let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let take_one f =
  {
    run =
      (fun input success failure ->
        if Input.can_get ~dist:0 input && f (Input.get ~dist:0 input) then
          let c = Input.get ~dist:0 input in
          success (Input.next ~dist:1 input) c
        else failure input []);
  }

let integer = take_while1 is_integer >>| int_of_string

let signed_integer =
  char '-' <|> return ' ' >>= fun s ->
  integer >>| fun i -> match s with ' ' -> i | _ -> -i
