open Format

(*******)
(* LIB *)
(*******)

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let seq_head s = match s () with
  | Seq.Nil -> failwith "sequence is empty"
  | Seq.Cons (v, _) -> v

let seq_push s x = Seq.append s (Seq.return x)

let str_of_char ch =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf ch;
  Buffer.contents buf
let char_of_str s =
  let d = String.get_utf_8_uchar s 0 in
  assert(Uchar.utf_decode_length d == String.length s);
  Uchar.utf_decode_uchar d

let sym_table = Hashtbl.create 100
let sym_table_val h = Hashtbl.find sym_table h
let sym_table_hash s =
  let hash = Hashtbl.hash s in
  let () = if Hashtbl.mem sym_table hash then
             assert (String.equal (Hashtbl.find sym_table hash) s)
           else
             let () = Hashtbl.add sym_table hash s in
             ()
  in
  hash

module rec V : sig
         type scope_t = (int, V.t) Hashtbl.t
         type scopes_t = scope_t list
         type t = Null
                | Bool of bool
                | Int of int
                | Char of Uchar.t
                | Sym of int
                | Set of S.t
                | Map of t M.t
                | List of t array
                | Tuple of t array
                | Str of string
                | Builtin of (t array -> t)
                | Func of fn_t
                | Macro of fn_t
                | Object of (type_t * t array)
                | Type of type_t
                | BuiltinType of int
         and fn_t = (scopes_t * t * t array)
         and type_t = int
         val compare : t -> t -> int
         val show : t -> string
         val map : (t * t) Seq.t -> t
         val set : t Seq.t -> t
         val sym : string -> t
         val list : t Seq.t -> t
         val char : string -> t
         val equal : t -> t -> bool
       end = struct

  type builtin_t = V.t array -> V.t
  type scope_t = (int, V.t) Hashtbl.t
  type scopes_t = scope_t list

  type t = Null
         | Bool of bool
         | Int of int
         | Char of Uchar.t
         | Sym of int
         | Set of S.t
         | Map of t M.t
         | List of t array
         | Tuple of t array
         | Str of string
         | Builtin of builtin_t
         | Func of fn_t
         | Macro of fn_t
         | Object of (type_t * t array)
         | Type of type_t
         | BuiltinType of int
  and fn_t = (scopes_t * V.t * V.t array)
  and type_t = int

  let compare = compare

  let show_type t = Hashtbl.find sym_table t

  let rec show = function
    | Null -> "()"
    | Bool b -> if b then "⊤" else "⊥"
    | Int i -> sprintf "%i" i
    | Char c -> sprintf "\"%s" (str_of_char c)
    | Sym hash -> sprintf "‘%s’" (Hashtbl.find sym_table hash)
    | Set s -> "{" ^ (String.concat ", " (List.of_seq (Seq.map show (S.to_seq s)))) ^ "}"
    | Map m -> "{" ^ (String.concat ", " (List.of_seq (Seq.map (fun (k, v) -> show k ^ " → " ^ show v) (M.to_seq m)))) ^ "}"
    | List l -> "⟨" ^ (String.concat ", " (List.of_seq (Seq.map show (Array.to_seq l)))) ^ "⟩"
    | Tuple l -> "(" ^ (String.concat ", " (List.of_seq (Seq.map show (Array.to_seq l)))) ^ ")"
    | Str s -> sprintf "\"%s\"" (String.escaped s)
    | Builtin _ -> "<builtin>"
    | Func _ -> "<function>"
    | Macro _ -> "<macro>"
    | Object (ty, l) ->
       show_type ty ^ "[" ^ (String.concat ", " (List.of_seq (Seq.map show (Array.to_seq l)))) ^ "]"
    | Type ty -> show_type ty
    | BuiltinType hash -> sym_table_val hash

  let map s = Map (M.of_seq s)
  let set s = Set (S.of_seq s)

  let sym s =
    Sym (sym_table_hash s)
  let list l = V.List (Array.of_seq l)
  let char s = V.Char (char_of_str s)

  let equal v1 v2 = v1 = v2

end
   and M : sig
     include Map.S with type key := V.t
   end = Map.Make(V)
   and S : sig
     include Set.S with type elt := V.t
   end = Set.Make(V)

let null_t = V.BuiltinType (sym_table_hash "Null")
let bool_t = V.BuiltinType (sym_table_hash "Bool")
let int_t = V.BuiltinType (sym_table_hash "Int")
let char_t = V.BuiltinType (sym_table_hash "Char")
let sym_t = V.BuiltinType (sym_table_hash "Sym")
let list_t = V.BuiltinType (sym_table_hash "List")
let str_t = V.BuiltinType (sym_table_hash "Str")
let type_t = V.BuiltinType (sym_table_hash "Type")

let typeof = function
  | V.Null -> null_t
  | V.Bool _ -> bool_t
  | V.Int _ -> int_t
  | V.Char _ -> char_t
  | V.Sym _ -> sym_t
  | V.Str _ -> str_t
  | V.List _ -> list_t
  | V.Object (ty, _) -> V.Type ty
  | v -> failwith ("unimplemented typeof" ^ (V.show v))

let unbox_bool = function
  | V.Bool b -> b
  | _ -> failwith "not a bool"

let unbox_int = function
  | V.Int i -> i
  | _ -> failwith "not an int"

let unbox_char = function
  | V.Char c -> c
  | _ -> failwith "not a char"

let unbox_sym = function
  | V.Sym hash -> hash
  | _ -> failwith "not a sym"

let unbox_tuple = function
  | V.Tuple l -> l
  | _ -> failwith "not a tuple"

let unbox_list = function
  | V.List l -> l
  | _ -> failwith "not a list"

let unbox_set = function
  | V.Set s -> s
  | _ -> failwith "not a set"

let unbox_map = function
  | V.Map m -> m
  | _ -> failwith "not a map"

let unbox_str = function
  | V.Str s -> s
  | _ -> failwith "not a str"

let unbox_builtin = function
  | V.Builtin f -> f
  | _ -> failwith "not a builtin"

let unbox_func = function
  | V.Func f -> f
  | _ -> failwith "not a func"

let unbox_macro = function
  | V.Macro f -> f
  | _ -> failwith "not a macro"

let unbox_object = function
  | V.Object o -> o
  | _ -> failwith "not an object"

let unbox_type = function
  | V.Type t -> t
  | _ -> failwith "not a type"

let unbox_builtin_type = function
  | V.BuiltinType t -> t
  | _ -> failwith "not a builtin type"


let fn_sym = sym_table_hash "fn"
let if_sym = sym_table_hash "if"
let let_sym = sym_table_hash "let"
let set_sym = sym_table_hash "set"
let type_sym = sym_table_hash "type"
let list_sym = sym_table_hash "list"
let brace_sym = sym_table_hash "brace"
let while_sym = sym_table_hash "while"
let quote_sym = sym_table_hash "quote"
let match_sym = sym_table_hash "match"
let macro_sym = sym_table_hash "macro"
let quasiquote_sym = sym_table_hash "quasiquote"
let unquote_sym = sym_table_hash "unquote"
let unquote_splicing_sym = sym_table_hash "unquote-splicing"

(*********)
(* PARSE *)
(*********)

module CharSet = Set.Make(Uchar)

let charset s =
  let i = ref 0 in
  let rec ret () =
    let i_val = !i in
    if i_val >= String.length s
    then Seq.Nil
    else begin
        let d = String.get_utf_8_uchar s i_val in
        i := i_val + Uchar.utf_decode_length d;
        Seq.Cons (Uchar.utf_decode_uchar d, ret)
      end
  in
  CharSet.of_seq ret

let cs_N = charset "0123456789"
let cs_X = CharSet.union cs_N (charset "abcdef")
let cs_R = charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let cs_V = charset "𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧𝐴𝐵𝐶𝐷𝐸𝐹𝐺𝐻𝐼𝐽𝐾𝐿𝑀𝑁𝑂𝑃𝑄𝑅𝑆𝑇𝑈𝑉𝑊𝑋𝑌𝑍𝛼𝛽𝛾𝛿𝜀𝜁𝜂𝜃𝜄𝜅𝜆𝜇𝜈𝜉𝜊𝜋𝜌𝜎𝜏𝜐𝜙𝜒𝜓𝜔𝛢𝛣𝛤𝛥𝛦𝛧𝛨𝛩𝛪𝛫𝛬𝛭𝛮𝛯𝛰𝛱𝛲𝛴𝛵𝛶𝛷𝛸𝛹𝛺"
let cs_S = charset "_+-*×/<>=≠≤≥?!&#@^¬.,:…≠←→↑↓%∧∨"
let cs_W = charset " \t\n"
let cs_I = List.fold_left CharSet.union CharSet.empty [cs_N; cs_R; cs_S; charset "ʹʺ"]

let ch_0 = Uchar.of_int 0

module Parser = struct
  type state = {string: string; mutable index: int}

  exception Parse_Error

  let init s = {string = s; index = 0}

  let look q =
    if q.index < String.length q.string then
      Uchar.utf_decode_uchar (String.get_utf_8_uchar q.string q.index)
    else
      ch_0
  let next q =
    let d = String.get_utf_8_uchar q.string q.index in
    q.index <- q.index + Uchar.utf_decode_length d

  let collect_same q cs = 
   let init_index = q.index in
    while CharSet.mem (look q) cs do
      next q
    done;
    String.sub q.string init_index (q.index - init_index)

  let rec skip_whitespace q =
    let _ = collect_same q cs_W in
    if look q == char_of_str ";" then (
      let () = next q in
      let ch = ref (look q) in
      while not (!ch == Uchar.of_char '\n' || !ch == ch_0) do
        next q;
        ch := look q
      done;
      if not (!ch == ch_0) then (next q; skip_whitespace q) else ();
    )
    else ()

  let escape_table =
    let pairs =
      [ ('n', '\n')
      ; ('t', '\t')
      ; ('0', '\000')
      ]
    in
    Hashtbl.of_seq @@ Seq.map (fun (k, v) -> (Uchar.of_char k, Uchar.of_char v)) @@  List.to_seq pairs

  let parse_char end_ch q =
    let ch = look q in
    if ch == end_ch then (
      None
    )
    else if ch == Uchar.of_char '\\' then (
      let () = next q in
      let ch2 = look q in
      let () = next q in
      if ch2 == Uchar.of_char '\\' || ch2 == end_ch then
        Some ch2
      else if Hashtbl.mem escape_table ch2 then
        Some (Hashtbl.find escape_table ch2)
      else
        failwith "unknown escape"
    )
    else (
      let () = next q in
      Some ch
    )

  let parse_string end_ch q =
    let buf = Buffer.create 4 in
    let rec loop () =
      match parse_char end_ch q with
      | Some c -> let () = Buffer.add_utf_8_uchar buf c in loop ()
      | None -> ()
    in
    let () = loop () in
    let () = next q in
    Buffer.contents buf

  let rec parse_unit q =
    let ch = look q in
    if CharSet.mem ch cs_N then (
      if ch == Uchar.of_char '0' then (
        next q;
        let ch = look q in
        if ch == Uchar.of_char 'x' then (
          next q;
          V.Int (int_of_string @@ ("0x" ^ collect_same q cs_X))
        )
        else if CharSet.mem ch cs_N then
          V.Int (int_of_string @@ collect_same q cs_N)
        else
          V.Int 0
      )
      else
        V.Int (int_of_string @@ collect_same q cs_N)
    )
    else if CharSet.mem ch cs_R || CharSet.mem ch cs_S then
      V.sym @@ collect_same q cs_I
    else if CharSet.mem ch cs_V then
      let () = next q in
      V.sym @@ str_of_char ch
    else if ch == char_of_str "“" then
      let () = next q in
      let s = parse_string (char_of_str "”") q in
      V.Str s
    else if ch == char_of_str "‘" then
      let () = next q in
      let s = parse_string (char_of_str "’") q in
      V.sym s
    else if ch == Uchar.of_char '(' then
      let () = next q in
      let ret = V.list @@ run q (Uchar.of_char ')') in
      let () = next q in
      ret
    else if ch == Uchar.of_char '[' then
      let () = next q in
      let ret = V.list @@ Seq.cons (V.Sym list_sym) @@ run q (Uchar.of_char ']') in
      let () = next q in
      ret
    else if ch == Uchar.of_char '{' then
      let () = next q in
      let ret = V.list @@ Seq.cons (V.Sym brace_sym) @@ run q (Uchar.of_char '}') in
      let () = next q in
      ret
    else if ch == Uchar.of_char '\'' then
      let () = next q in
      let v = parse_unit q in
      V.List [| V.Sym quote_sym; v |]
    else if ch == Uchar.of_char '`' then
      let () = next q in
      let v = parse_unit q in
      V.List [| V.Sym quasiquote_sym; v |]
    else if ch == char_of_str "ʹ" then
      let () = next q in
      V.List [| V.Sym unquote_sym; parse_unit q |]
    else if ch == char_of_str "ʺ" then
      let () = next q in
      V.List [| V.Sym unquote_splicing_sym; parse_unit q |]
    else if ch == char_of_str "◎" then
      let () = next q in
      V.Null
    else if ch == char_of_str "⊤" then
      let () = next q in
      V.Bool true
    else if ch == char_of_str "⊥" then
      let () = next q in
      V.Bool false
    else if ch == Uchar.of_char '\"' then
      let () = next q in
      (match parse_char ch_0 q with
       | Some c -> V.Char c
       | None -> failwith "expected char literal"
      )
    else raise Parse_Error

  and run q end_char () =
    let () = skip_whitespace q in
    let ch = look q in
    if ch == end_char then
      Seq.Nil
    else begin
        let v = parse_unit q in
        Seq.Cons (v, run q end_char)
      end

  let parse s = run (init s) ch_0

end

(************)
(* Matching *)
(************)

module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

let map_right_merge = M.union (fun _ _ v -> Some v)
let int_map_right_merge = IntMap.union (fun _ _ v -> Some v)

module Match = struct
  
  type input_t = {offset: int; values: V.t array}
  type state_t = {ip: int; stack: V.t list; capture: V.t IntMap.t}
  type ret_t = V.t IntMap.t list
  type fn_t = Fn of (fn_t -> state_t -> input_t -> ret_t -> (state_t * (V.t -> V.t IntMap.t option)) list -> state_t list -> ret_t) array

  let match_insts (Fn x) = x

  let rec exec fn tokens ret inactive = function
    | s :: active ->
       let f = Array.get (match_insts fn) s.ip in
       (f fn s tokens ret inactive active)
    | [] ->
       if tokens.offset >= Array.length tokens.values then
         ret
       else
         let t = Array.get tokens.values tokens.offset in
         exec fn {tokens with offset = tokens.offset + 1} ret [] @@
           List.filter_map (fun (s, f) -> match f t with | Some b -> Some {s with capture = int_map_right_merge s.capture b} | None -> None) inactive

  let extract_match m l =
    let ret = exec m {offset = 0; values = l} [] [] [{ip = 0; stack = []; capture = IntMap.empty}] in
    (match List.length ret with
     | 0 -> None
     | 1 -> Some (List.hd ret)
     | _ -> failwith "ambiguous match"
    )

  let imatch f = fun fn s tokens ret inactive active ->
    exec fn tokens ret (({s with ip = s.ip + 1}, f) :: inactive) active

  let imatch_lit c = imatch (fun x -> if x = c then Some IntMap.empty else None)
  let imatch_any = imatch (fun x -> Some IntMap.empty)

  let ifork i = fun fn s tokens ret inactive active ->
    exec fn tokens ret inactive ({s with ip = s.ip + 1} :: {s with ip = i} :: active)

  let iret = fun fn s tokens ret inactive active ->
    exec fn tokens (if Array.length tokens.values <= tokens.offset then s.capture :: ret else ret) inactive active

  let ibr i = fun fn s tokens ret inactive active ->
    exec fn tokens ret inactive ({s with ip = i} :: active)

  let icapt_start = fun fn s tokens ret inactive active ->
    exec fn tokens ret inactive ({s with ip = s.ip + 1; stack = V.Int tokens.offset :: s.stack} :: active)

  let icapt_end hash = fun fn s tokens ret inactive active ->
    let start = unbox_int (List.hd s.stack) in
    let capt = V.List (Array.sub tokens.values start (tokens.offset - start)) in
    exec fn tokens ret inactive ({ip = s.ip + 1; stack = List.tl s.stack; capture = IntMap.add hash capt s.capture} :: active)

  let icapt_last hash = fun fn s tokens ret inactive active ->
    let capt = Array.get tokens.values (tokens.offset - 1) in
    exec fn tokens ret inactive ({s with ip = s.ip + 1; capture = IntMap.add hash capt s.capture} :: active)

  (* let __matches = Hashtbl.create 1024 *)

  let rec compile eval =
    let bind hash v = Some (IntMap.singleton hash v) in
    let literal w v = if V.equal v w then Some IntMap.empty else None in
    let list fn v =
      match v with
      | V.List l -> extract_match (Fn fn) l
      | _ -> None
    in
    let type_match t_v fn v =
      match t_v with
      | V.BuiltinType _ ->
         if V.equal t_v (typeof v) then (
           if V.equal t_v null_t then
            extract_match (Fn fn) [||]
           else
            extract_match (Fn fn) [|v|]
         ) else None
      | V.Type _ -> (match v with
                     | V.Object (t, l) ->
                        if V.equal t_v (V.Type t) then
                          extract_match (Fn fn) l
                        else None
                     | _ -> None
                    )
      | _ -> failwith "expected type value in # pattern"
    in
    (* fun x -> (if Hashtbl.mem __matches x then () else (Hashtbl.add __matches x 1; eprintf "%s\n" (V.show x))); match x with *)
    function
    | V.Sym hash -> if hash == sym_table_hash "." then
                      (fun v -> Some (IntMap.empty))
                    else
                      bind hash
    | V.List l ->
       let delegate v =
         let m = compile eval v in
         (1, Seq.return @@ imatch (fun v -> m v))
       in
       let rec comp start = function
         | V.List l ->
            assert (Array.length l > 0);
            if l.(0) = V.sym "*" then
              let (len, s) = loop (start + 1) @@ Seq.drop 1 @@ Array.to_seq l in
              (len + 2, seq_push (Seq.cons (ifork @@ start + len + 2) s) (ibr start))
            else if l.(0) = V.sym ":" then
              let () = assert (Array.length l > 2) in
              let (len, s) = loop (start + 1) @@ Seq.drop 2 @@ Array.to_seq l in
              (len + 2, seq_push (Seq.cons icapt_start s) (icapt_end (unbox_sym l.(1))))
            else if l.(0) = V.sym "…" then
              let () = assert (Array.length l = 2) in
              let hash = unbox_sym l.(1) in
              let inst fn s tokens ret inactive active =
                let rest = Array.sub
                             tokens.values
                             tokens.offset
                             (Array.length tokens.values - tokens.offset)
                in
                let capt = IntMap.add hash (V.List rest) s.capture in
                exec fn tokens (capt :: ret) inactive active
              in
              (1, Seq.return inst)
            else
              delegate @@ V.List l
         | v -> delegate v
       and loop start s = match s () with
         | Seq.Nil -> (0, Seq.empty)
         | Seq.Cons (v, s) -> 
            let (len, t) = comp start v in
            let (len_r, t_r) = loop (start + len) s in
            (len + len_r, Seq.append t t_r)
       and of_seq s_v = 
         let (_, s) = loop 0 s_v in
         Array.of_seq @@ seq_push s iret
       in
       assert (Array.length l > 0);
       if V.equal l.(0) @@ V.sym "!" then
         let () = assert (Array.length l == 2) in
         literal (eval l.(1))
       else if V.equal l.(0) @@ V.sym "@" then
         let () = assert (Array.length l == 3) in
         let hash = unbox_sym l.(1) in
         let m = compile eval l.(2) in
         (fun v -> match m v with | Some b -> Some (IntMap.add hash v b) | None -> None)
       else if V.equal l.(0) @@ V.Sym quote_sym then
         let () = assert (Array.length l == 2) in
         literal l.(1)
       else if V.equal l.(0) @@ V.sym "#" then
         let () = assert (Array.length l >= 2) in
         type_match (eval l.(1)) @@ of_seq @@ Seq.drop 2 @@ Array.to_seq l
       else if V.equal l.(0) @@ V.Sym list_sym then
         list (of_seq @@ Seq.drop 1 @@ Array.to_seq l)
       else
         failwith "ivalid sexp in match"
    | v -> literal v

end


(********)
(* EXEC *)
(********)

type scope_t = V.scope_t
type scopes_t = V.scopes_t

exception Not_in_Scope of string

let new_scope () = Hashtbl.create 8

let rec get_scope_with hash scopes = match scopes with
  | [] -> raise (Not_in_Scope (sym_table_val hash))
  | x :: xs -> if Hashtbl.mem x hash then
                 x
               else
                 get_scope_with hash xs

let lookup_scope scopes hash =
  Hashtbl.find (get_scope_with hash scopes) hash

let set_scope scopes hash v =
  Hashtbl.replace (get_scope_with hash scopes) hash v

let rec seq_ints n m () =
  if n == m then
    Seq.Nil
  else
    Seq.Cons (n, seq_ints (n + 1) m)


let rec call_fn (scopesP, m_v, body) args =
  let m = Match.compile (eval scopesP) m_v in
  let Some b = m (V.List args) in
  let scope = Hashtbl.of_seq @@ IntMap.to_seq b in
  let scopesPP = scope :: scopesP in
  let ret = ref V.Null in
  let () = Array.iter (fun expr -> ret := eval scopesPP expr) body in
  !ret

and call v l = 
  match v with
  | V.Builtin f -> f l
  | V.Func fn -> call_fn fn l
  | _ -> failwith "can only call builtin or func"

and eval scopes expr = match expr with
  | V.Null -> expr
  | V.Bool _ -> expr
  | V.Int _ -> expr
  | V.Char _ -> expr
  | V.Str _ -> expr
  | V.Sym hash -> lookup_scope scopes hash
  | V.Tuple l -> 
     let ret = ref V.Null in
     let () = Seq.iter (fun x -> ret := eval scopes x) @@ Array.to_seq l in
     !ret
  | V.List [||] -> V.Null
  | V.List l ->
     let funcall () =
       let f_val = eval scopes l.(0) in
       let strict_args () = Array.of_seq @@ Seq.map (eval scopes) @@ Seq.drop 1 @@ Array.to_seq l in
       let escaped_args () = Array.of_seq @@ Seq.drop 1 @@ Array.to_seq l in
       (match f_val with
        | V.Builtin f -> f (strict_args ())
        | V.Func ft -> call_fn ft (strict_args ())
        | V.Macro ft -> eval scopes @@ call_fn ft (escaped_args ())
        | V.Type ty -> V.Object (ty, strict_args ())
        | _ -> failwith "value is not a function"
       )
     in
     let bind scope hash w =
       let () = assert (not @@ Hashtbl.mem scope hash) in
       let () = Hashtbl.replace scope hash w in
       ()
     in
     (match l.(0) with
      | V.Sym hash ->
         if hash = fn_sym then
           let () = assert (Array.length l >= 3) in
           let body = Array.of_seq @@ Seq.drop 2 @@ Array.to_seq l in
           V.Func (scopes, l.(1), body)
         else if hash = macro_sym then
           let () = assert (Array.length l >= 3) in
           let body = Array.of_seq @@ Seq.drop 2 @@ Array.to_seq l in
           V.Macro (scopes, l.(1), body)
         else if hash = let_sym then
           let () = assert (Array.length l = 3) in
           let m = Match.compile (eval scopes) l.(1) in
           let v = eval scopes l.(2) in
           let scope = List.hd scopes in
           (match m v with
            | None -> failwith "failed match in let binding"
            | Some bindings ->
               let () = IntMap.iter (bind scope) bindings in
               V.Null
           )
         else if hash = set_sym then
           let () = assert (Array.length l = 3) in
           let m = Match.compile (eval scopes) l.(1) in
           let v = eval scopes l.(2) in
           (match m v with
            | None -> failwith "failed match in set binding"
            | Some bindings ->
               let () = IntMap.iter (set_scope scopes) bindings in
               V.Null
           )
         else if hash = while_sym then
           let () = assert (Array.length l >= 3) in
           let cond = l.(1) in
           let body = Seq.drop 2 @@ Array.to_seq l in
           while unbox_bool (eval scopes cond) do
             let scope = new_scope () in
             Seq.iter (fun x -> let _ = eval (scope :: scopes) x in ()) body
           done;
           V.Null
         else if hash = brace_sym then
           let () = assert (Array.length l >= 1) in
           let body = Seq.drop 1 @@ Array.to_seq l in
           let scope = new_scope () in
           let ret = ref V.Null in
           let () = Seq.iter (fun x -> ret := eval (scope :: scopes) x) body in
           !ret
         else if hash = if_sym then
           let () = assert (Array.length l == 4) in
           let cond = unbox_bool @@ eval scopes l.(1) in
           eval scopes (if cond then l.(2) else l.(3))
         else if hash = match_sym then
           let () = assert (Array.length l >= 3) in
           let v = eval scopes l.(1) in
           let ret = ref None in
           for i = 2 to (Array.length l) - 1 do
             let row = unbox_list l.(i) in
             let () = assert (Array.length row >= 1) in
             let m = Match.compile (eval scopes) row.(0) in
             (match m v with
              | None -> ()
              | Some b ->
                 if Option.is_some !ret then
                   failwith "found multiple matches";
                 let body = Seq.drop 1 @@ Array.to_seq row in
                 let inner_scopes = (Hashtbl.of_seq @@ IntMap.to_seq b) :: scopes in
                 Seq.iter (fun w -> ret := Some (eval inner_scopes w)) body
             )
           done;
           (match !ret with
            | None -> failwith "no matches found"
            | Some w -> w
           )
         else if hash = quote_sym then
           let () = assert (Array.length l == 2) in
           l.(1)
         else if hash = quasiquote_sym then
           let () = assert (Array.length l == 2) in
           let rec descend = function
             | V.List l ->
                if Array.length l >= 1 && l.(0) = V.Sym unquote_sym then
                  let () = assert (Array.length l == 2) in
                  Seq.return @@ eval scopes l.(1)
                else if Array.length l >= 1 && l.(0) = V.Sym unquote_splicing_sym then
                  let l = unbox_list @@ eval scopes l.(1) in
                  Array.to_seq l
                else
                  Seq.return @@ V.List (Array.of_seq @@ Seq.concat @@ Seq.map descend @@ Array.to_seq l)
             | v -> Seq.return v
           in
           let ret = Array.of_seq @@ descend l.(1) in
           if Array.length ret == 1 then
             ret.(0)
           else
             V.List ret
         else if hash = type_sym then
           let () = assert (Array.length l == 2) in
           let hash = unbox_sym l.(1) in
           V.Type hash
         else
           funcall ()
      | _ -> funcall ())
  | _ -> failwith "unhandled type in eval"

let stringify v =
  match v with
  | V.Sym hash -> sym_table_val hash
  | V.Str s -> s
  | _ -> V.show v

let rec make_tuple l =
  let seq_tuple = function
    | V.Null -> Seq.empty
    | V.Tuple l -> Array.to_seq l
    | v -> Seq.return v
  in
  let vals = Array.of_seq @@ Seq.concat @@ Seq.map seq_tuple @@ Array.to_seq l in
  match Array.length vals with
  | 0 -> V.Null
  | 1 -> vals.(0)
  | _ -> V.Tuple vals

let builtins = ref (Hashtbl.create 0)
let () = builtins :=
  Hashtbl.of_seq @@
    Seq.append
      (Seq.map (fun (n, v) -> (sym_table_hash n, v))
       @@ List.to_seq
            [ ("null", V.Null)
            ; ("⊤", V.Bool true)
            ; ("⊥", V.Bool false)
            ; ("Null", null_t)
            ; ("Bool", bool_t)
            ; ("Int", int_t)
            ; ("Char", char_t)
            ; ("Sym", sym_t)
            ; ("List", list_t)
            ; ("Str", str_t)
            ; ("Type", type_t)
            ]
      ) @@
    Seq.map (fun (n, b) -> (sym_table_hash n, V.Builtin b)) @@
      List.to_seq
        [ ("+", fun l ->
               V.Int (unbox_int l.(0) + unbox_int l.(1))
          )
        ; ("-", fun l ->
               V.Int (unbox_int l.(0) - unbox_int l.(1))
          )
        ; ("×", fun l ->
               V.Int (unbox_int l.(0) * unbox_int l.(1))
          )
        ; ("/", fun l ->
               V.Int (unbox_int l.(0) / unbox_int l.(1))
          )
        ; ("%", fun l ->
               V.Int (unbox_int l.(0) mod unbox_int l.(1))
          )
        ; ("<", fun l ->
               V.Bool (unbox_int l.(0) < unbox_int l.(1))
          )
        ; (">", fun l ->
               V.Bool (unbox_int l.(0) > unbox_int l.(1))
          )
        ; ("≤", fun l ->
               V.Bool (unbox_int l.(0) <= unbox_int l.(1))
          )
        ; ("≥", fun l ->
               V.Bool (unbox_int l.(0) >= unbox_int l.(1))
          )
        ; ("=", fun l ->
               V.Bool (V.equal l.(0) l.(1))
          )
        ; ("≠", fun l ->
               V.Bool (not (V.equal l.(0) l.(1)))
          )
        ; ("∧", fun l ->
               V.Bool (unbox_bool l.(0) && unbox_bool l.(1))
          )
        ; ("∨", fun l ->
               V.Bool (unbox_bool l.(0) || unbox_bool l.(1))
          )
        ; ("¬", fun l ->
               V.Bool (not (unbox_bool l.(0)))
          )
        ; ("print", fun l ->
               let () = printf "%s\n" (V.show l.(0)) in
               V.Null
          )
        ; ("debug", fun l ->
               let () = eprintf "%s\n" (V.show l.(0)) in
               V.Null
          )
        ; ("show", fun l ->
               V.Str (V.show l.(0))
          )
        ; ("sum", fun l -> V.Int (Seq.fold_left (+) 0 @@ Seq.map unbox_int @@ Array.to_seq (unbox_list l.(0))))
        ; ("max", fun l -> V.Int (Seq.fold_left (fun x y -> if x > y then x else y) (unbox_int l.(0)) @@ Seq.map unbox_int @@ Array.to_seq l))
        ; ("min", fun l -> V.Int (Seq.fold_left (fun x y -> if x < y then x else y) (unbox_int l.(0)) @@ Seq.map unbox_int @@ Array.to_seq l))
        ; ("str", fun l -> V.Str (stringify l.(0)))
        ; ("sym", fun l -> V.sym (unbox_str l.(0)))
        ; ("func", fun l -> V.Func ([!builtins], l.(0), unbox_list l.(1)))
        ; ("int", fun l -> V.Int (match l.(0) with | V.Int i -> i | V.Char ch -> Uchar.to_int ch | V.Sym hash -> hash | _ -> failwith "can not intify"))
        ; ("bytes", fun l ->
                    V.List (Array.of_seq @@ Seq.map (fun c -> V.Int (Char.code c)) @@ String.to_seq @@ unbox_str l.(0))
          )
        ; ("each", fun l ->
                   let () = Array.iter (fun v -> call l.(0) [|v|]; ()) @@ unbox_list l.(1) in
                   V.Null)
        ; ("filter", fun l ->
               V.List (Array.of_seq @@ Seq.filter (fun v -> unbox_bool @@ call l.(0) [|v|]) @@ Array.to_seq @@ unbox_list l.(1)))
        ; ("map", fun l ->
               V.List (Array.map (fun v -> call l.(0) [|v|]) @@ unbox_list l.(1)))
        ; ("mapi", fun l ->
               V.List (Array.mapi (fun i v -> call l.(0) [|v; (V.Int i)|]) @@ unbox_list l.(1)))
        ; ("len",  fun l ->
                   match l.(0) with
                   | V.List l2 -> V.Int (Array.length l2)
                   | V.Str s -> V.Int (String.length s)
          )
        ; ("concat", fun l ->
                     match l.(0) with
                     | V.Sym _ -> V.sym (String.concat "" (List.of_seq @@ Seq.map stringify @@ Array.to_seq l))
                     | V.Str _ -> V.Str (String.concat "" (List.of_seq @@ Seq.map unbox_str @@ Array.to_seq l))
                     | V.List _ -> V.List (Array.concat (List.of_seq @@ Seq.map unbox_list @@ Array.to_seq l))
                     | V.Map _ -> V.Map (map_right_merge (unbox_map l.(0)) (unbox_map l.(1)))
                     | _ -> failwith "concat: Type Error"
          )
        ; ("insert", fun l ->
                     match l.(0) with
                     | V.Map m -> V.Map (M.add l.(1) l.(2) m)
                     | _ -> failwith "insert: Type Error"
          )
        ; ("join", fun l ->
               V.Str (String.concat (unbox_str l.(0)) (List.of_seq @@ Seq.map unbox_str @@ Array.to_seq (unbox_list l.(1))))
          )
        ; ("sub", fun l ->
                  let start = unbox_int l.(1) in
                  let len = (unbox_int l.(2)) - start in
                  V.List (Array.sub (unbox_list l.(0)) start len)
          )
        ; ("puts", fun l ->
                   printf "%s" (unbox_str l.(0));
                   V.Null
          )
        ; ("apply", fun l ->
               call l.(0) @@ unbox_list l.(1))
        ; ("typeof", fun l -> typeof l.(0))
        ; ("alloc", fun l ->
                    let () = assert (Array.length l >= 2) in
                    let head = Array.of_seq @@ Seq.drop 2 @@ Array.to_seq l in
                    let made = Array.make (unbox_int l.(1)) V.Null in
                    let () = Array.blit head 0 made 0 (Array.length head) in
                    let t = l.(0) in
                    match t with
                    | V.BuiltinType _ -> if V.equal t list_t then
                                          V.List made
                                        else
                                          failwith ("can not allocate " ^ (V.show t))
                    | V.Type u -> V.Object (u, made)
          )
        ; ("list", fun l -> V.List l)
        ; ("pack", fun l -> match l.(0) with
                            | V.Null -> V.List [||]
                            | V.Tuple l -> V.List l
                            | v -> V.List [|v|]
          )
        ; ("Map", fun l ->
                  let s =
                    Seq.map (fun p -> let [|k; v|] = unbox_list p in (k, v))
                    @@ Array.to_seq l
                  in
                  V.map s
          )
        ; ("values", fun l -> V.List (Array.of_seq @@ Seq.map (fun (k, v) -> v) @@ M.to_seq @@ unbox_map l.(0)))
        ; ("keys", fun l -> V.List (Array.of_seq @@ Seq.map (fun (k, v) -> k) @@ M.to_seq @@ unbox_map l.(0)))
        ; ("items", fun l -> V.List (Array.of_seq @@ Seq.map (fun (k, v) -> V.List [|k; v|]) @@ M.to_seq @@ unbox_map l.(0)))
        ; ("error", fun l ->
                    let s = String.concat "" @@ List.of_seq @@ Seq.map stringify @@ Array.to_seq l in
                    failwith ("interpreter error: " ^ s)
          )
        ; ("has", fun l -> V.Bool (
               match l.(0) with
               | V.List p -> unbox_int l.(1) < Array.length p
               | V.Object (_, p) -> unbox_int l.(1) < Array.length p
               | V.Map m -> M.mem l.(1) m
               | _ -> failwith "can not get value from non-object, non-list"
          ))
        ; (".", fun l ->
               match l.(0) with
               | V.List p -> p.(unbox_int l.(1))
               | V.Object (_, p) -> p.(unbox_int l.(1))
               | V.Map m -> M.find l.(1) m
               | _ -> failwith "can not get value from non-object, non-list"
          )
        ; (",", fun l -> make_tuple l)
        ; ("←", fun l ->
               (match l.(0) with
               | V.List p -> p.(unbox_int l.(1)) <- l.(2)
               | V.Object (_, p) -> p.(unbox_int l.(1)) <- l.(2)
               | _ -> failwith "can not set value of non-object, non-list");
               V.Null
          )
        ]

let a = [|1|]
let () = a.(0) <- 2
let src = if Sys.argv.(1) = "-e" then Sys.argv.(2) else read_whole_file Sys.argv.(1)
let rec loop s =
  match s () with
  | Seq.Nil -> ()
  | Seq.Cons (x, s_p) ->
     if V.equal x @@ V.sym "___stage2___" then
       let _ = call (Hashtbl.find !builtins (sym_table_hash "stage2")) [|(V.List (Array.of_seq s_p))|] in
       ()
     else
       let _ = eval [!builtins] x in
       loop s_p
let () = loop @@ Parser.parse src
