
let chain (f : 'a -> unit) : 'a -> 'a =
  fun x -> f x ; x

module type Encoder_dict = sig
  type t
  val reset_code : int
  val make       : unit -> t
  val add        : t -> string -> unit
  val mem        : t -> string -> bool
  val find       : t -> string -> int
  val is_full    : t -> bool
  val reset      : t -> unit
end

module Encoder (Dict : Encoder_dict) = struct

  module State = struct
    type t =
      { seq   : string
      ; token : string }
  end

  module Change = struct
    type t =
      | Found     of found
      | Not_found of not_found
      | Overflow  of overflow
    and found =
      { seq : string }
    and not_found =
      { seq     : string
      ; new_seq : string
      ; code    : int }
    and overflow =
      { seq  : string
      ; code : int }
  end

  type mem     = string -> bool
  type find    = string -> int
  type is_full = unit -> bool

  let encode ~mem ~find ~is_full state =
    let { State. seq ; token } = state in
    let next_seq = seq ^ token in
    if mem next_seq then
      Change.Found
        { seq = next_seq }
    else if is_full () then
      Change.Overflow
        { seq = token
        ; code = find seq }
    else
      Change.Not_found
        { seq = token
        ; new_seq = next_seq
        ; code = find seq }

  type env =
    { mutable seq : string
    ; dict        : Dict.t }

  let make_env () =
    { seq = "" ; dict = Dict.make () }

  let update env = function
  | Change.Found { seq } ->
    env.seq <- seq
  | Change.Not_found { seq ; new_seq ; code } ->
    env.seq <- seq ;
    Dict.add env.dict new_seq
  | Change.Overflow { seq ; code } ->
    env.seq <- seq ;
    Dict.reset env.dict

  let emit = function
  | Change.Found     _ -> []
  | Change.Not_found v -> [ v.code ]
  | Change.Overflow  v -> [ v.code ; Dict.reset_code ]

  let make () =
    let env = make_env () in
    let as_state token = { State. seq = env.seq ; token = token } in
    let encode = encode
      ~mem:(Dict.mem env.dict)
      ~find:(Dict.find env.dict)
      ~is_full:(fun _ -> Dict.is_full env.dict)
    in
      fun token -> token
        |> as_state
        |> encode
        |> chain (update env)
        |> emit

end

(* ================================================================ *)

module Encoder_test = struct

  module Dict_stub = struct
    type t = unit
    let reset_code = 42
    let make () = ()
    let add d s = ()
    let mem d s = false
    let find d s = 42
    let is_full d = false
    let reset d = ()
  end

  module E = Encoder (Dict_stub)

  let name = "encode"

  let _ =
    "TEST : " ^ name |> print_endline;

    let encode = E.encode
      ~mem:(fun s -> s = "A")
      ~find:(fun _ -> 13)
      ~is_full:(fun _ -> false)
    in
      ( match encode { seq = "" ; token = "A" } with
      | E.Change.Found { seq } ->
        assert (seq = "A")
      | _ -> assert false );

      ( match encode { seq = "A" ; token = "A" } with
      | E.Change.Not_found { seq ; new_seq ; code } ->
        assert (seq = "A") ;
        assert (new_seq = "AA") ;
        assert (code = 13)
      | _ -> assert false );

      ( match encode { seq = "A" ; token = "B" } with
      | E.Change.Not_found { seq ; new_seq ; code } ->
        assert (seq = "B") ;
        assert (new_seq = "AB") ;
        assert (code = 13)
      | _ -> assert false );

    let encode = E.encode
      ~mem:(fun s -> s = "A")
      ~find:(fun _ -> 13)
      ~is_full:(fun _ -> true)
    in
      ( match encode { seq = "" ; token = "A" } with
      | E.Change.Found { seq } ->
        assert (seq = "A")
      | _ -> assert false );

      ( match encode { seq = "A" ; token = "A" } with
      | E.Change.Overflow { seq ; code } ->
        assert (seq = "A") ;
        assert (code = 13)
      | _ -> assert false );

      ( match encode { seq = "A" ; token = "B" } with
      | E.Change.Overflow { seq ; code } ->
        assert (seq = "B") ;
        assert (code = 13)
      | _ -> assert false );

    (* Cyrillic *)
    let encode = E.encode
      ~mem:(fun s -> s = "Ц")
      ~find:(fun _ -> 13)
      ~is_full:(fun _ -> false)
    in
      ( match encode { seq = "" ; token = "Ц" } with
      | E.Change.Found { seq } ->
        assert (seq = "Ц")
      | _ -> assert false );

      ( match encode { seq = "Ц" ; token = "Ц" } with
      | E.Change.Not_found { seq ; new_seq ; code } ->
        assert (seq = "Ц") ;
        assert (new_seq = "ЦЦ") ;
        assert (code = 13)
      | _ -> assert false );

      ( match encode { seq = "Ц" ; token = "Ж" } with
      | E.Change.Not_found { seq ; new_seq ; code } ->
        assert (seq = "Ж") ;
        assert (new_seq = "ЦЖ") ;
        assert (code = 13)
      | _ -> assert false );

    (* Japanese 絶対 *)
    let encode = E.encode
      ~mem:(fun s -> s = "絶")
      ~find:(fun _ -> 13)
      ~is_full:(fun _ -> false)
    in
      ( match encode { seq = "" ; token = "絶" } with
      | E.Change.Found { seq } ->
        assert (seq = "絶")
      | _ -> assert false );

      ( match encode { seq = "絶" ; token = "絶" } with
      | E.Change.Not_found { seq ; new_seq ; code } ->
        assert (seq = "絶") ;
        assert (new_seq = "絶絶") ;
        assert (code = 13)
      | _ -> assert false );

      ( match encode { seq = "絶" ; token = "対" } with
      | E.Change.Not_found { seq ; new_seq ; code } ->
        assert (seq = "対") ;
        assert (new_seq = "絶対") ;
        assert (code = 13)
      | _ -> assert false );

    "PASS : " ^ name |> print_endline

end

(* ================================================================ *)

module type Token_encoder = sig
  val make : unit -> string -> int list
end

module Encode (Enc : Token_encoder) = struct

  let string_to_list s =
    let codes = ref [] in
    let push c = codes := c :: !codes in
    let push_all = List.iter push in
    let encode = Enc.make () in
    let as_token c = String.make 1 c in
    let f c = c |> as_token |> encode |> push_all in
    String.iter f s ;
    f '#' ;
    List.rev !codes

end

(* ================================================================ *)

module type Encode_conf = sig
  val capacity : int
  val alphabet : string
end

module Encode_dict (Conf : Encode_conf) = struct

  let _ =
    let cap = Conf.capacity in
    let alpha_len = String.length Conf.alphabet in
    assert (cap > 0) ;
    assert (alpha_len <= cap) ;
    let module CS = Set.Make (Char) in
    let unique = Conf.alphabet |> String.to_seq |> CS.of_seq in
    assert (alpha_len = CS.cardinal unique)

  type t = (string, int) Hashtbl.t

  let reset_code = Conf.capacity + 1

  let reset dict =
    Hashtbl.clear dict ;
    let map (i, c) = (String.make 1 c, i+1) in
    Conf.alphabet
    |> String.to_seqi
    |> Seq.map map
    |> Hashtbl.add_seq dict

  let make () =
    let dict = Hashtbl.create Conf.capacity in
    reset dict ;
    dict

  let is_full dict =
    Hashtbl.length dict = Conf.capacity

  exception Key_duplicate
  exception Capacity_overflow

  let add dict key =
    if Hashtbl.mem dict key then raise Key_duplicate ;
    if is_full dict then raise Capacity_overflow ;
    Hashtbl.add dict key (Hashtbl.length dict + 1)

  let mem = Hashtbl.mem

  exception Key_not_found

  let find dict key =
    match Hashtbl.find_opt dict key with
    | Some v -> v
    | None   -> raise Key_not_found

end

(* ================================================================ *)

module Encode_test = struct

  let str xs =
      "[" ^
      (  xs
      |> List.map string_of_int
      |> String.concat "; " ) ^
      "]"

  let _ =
    let module Dict = Encode_dict (struct
      let capacity = 42
      let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    end) in

    let module Encode = Encode (Encoder (Dict)) in

    let name = "string_to_list" in
    "TEST : " ^ name |> print_endline ;

    let input = "TOBEORNOTTOBEORTOBEORNOT" in
    let expected =
      [ 20; 15; 2; 5; 15; 18; 14; 15
      ; 20; 27; 29; 31; 36; 30; 32; 34 ]
    in
    let actual = Encode.string_to_list input in
    if List.equal (=) actual expected then
      "PASS : " ^ name |> print_endline
    else begin
      "FAIL : " ^ name |> print_endline ;
      "    expected: " ^ str expected |> print_endline ;
      "    actual:   " ^ str actual |> print_endline
    end

  let _ =
    let module Dict = Encode_dict (struct
      let capacity = 26
      let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    end) in

    let module Encode = Encode (Encoder (Dict)) in

    let name = "string_to_list (with overflow)" in
    "TEST : " ^ name |> print_endline ;

    let input = "TOBETO" in
    let expected = [20; 27; 15; 27; 2; 27; 5; 27; 20; 27; 15; 27] in
    let actual = Encode.string_to_list input in
    if List.equal (=) actual expected then
      "PASS : " ^ name |> print_endline
    else begin
      "FAIL : " ^ name |> print_endline ;
      "    expected: " ^ str expected |> print_endline ;
      "    actual:   " ^ str actual |> print_endline
    end

end

(* ================================================================ *)

module Encode_demo = struct

  module Dict = Encode_dict (struct
    let capacity = 42
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  end)

  module Encode = Encode (Encoder (Dict))

  let run input = input
    |> Encode.string_to_list
    |> List.iter (Printf.printf "%d ") ;
    print_endline ""

  let _ =
    run "TOTO" ;
    run "TOBEORNOTTOBEORTOBEORNOT" ;
    print_endline "20 15 2 5 15 18 14 15 20 27 29 31 36 30 32 34"

end

(* ================================================================ *)

module type Decoder_dict = sig
  type t
  val reset_code : int
  val make       : unit -> t
  val add        : t -> string -> unit
  val find_opt   : t -> int -> string option
  val reset      : t -> unit
  val size       : t -> int
end

module Decoder (Dict : Decoder_dict) = struct

  let append_first w s =
    w ^ String.sub s 0 1

  module State = struct
    type t =
      { seq  : string
      ; code : int }
  end

  module Change = struct
    type t =
      | First of string
      | Found of found
      | Reset
      | Not_found
    and found =
      { emit_seq : string
      ; dict_seq : string }
  end

  type find_opt = int -> string option
  type size     = unit -> int

  let decode ~find_opt ~size state =
    let { State. seq ; code } = state in
    if code = Dict.reset_code then
      Change.Reset
    else if seq = "" then
      match find_opt code with
      | Some v -> Change.First v
      | None   -> Change.Not_found
    else
      match find_opt code with
      | Some v ->
        Change.Found
          { emit_seq = v
          ; dict_seq = append_first seq v }
      | None ->
        if code = size () + 1 then
          let v = append_first seq seq in
          Change.Found
            { emit_seq = v
            ; dict_seq = append_first seq v }
        else
          Change.Not_found

  type env =
    { mutable seq : string
    ; dict        : Dict.t }

  let make_env () =
    { seq = "" ; dict = Dict.make () }

  exception Invalid_input

  let update env = function
  | Change.First seq ->
    env.seq <- seq ;
  | Change.Found { emit_seq ; dict_seq } ->
    Dict.add env.dict dict_seq ;
    env.seq <- emit_seq
  | Change.Reset ->
    Dict.reset env.dict ;
  | Change.Not_found ->
    raise Invalid_input

  let emit = function
  | Change.First seq -> [ seq ]
  | Change.Found v   -> [ v.emit_seq ]
  | Change.Reset     -> []
  | Change.Not_found -> raise Invalid_input

  let make () =
    let env = make_env () in
    let as_state code = { State. seq = env.seq ; code = code } in
    let decode = decode
      ~find_opt:(Dict.find_opt env.dict)
      ~size:(fun _ -> Dict.size env.dict)
    in
      fun code -> code
        |> as_state
        |> decode
        |> chain (update env)
        |> emit

end

(* ================================================================ *)

module Decoder_test = struct

  module Dict_stub = struct
    type t = unit
    let reset_code = 42
    let make () = ()
    let add d s = ()
    let find_opt d = function
      | 1 -> Some "A"
      | 2 -> Some "B"
      | 3 -> Some "AB"
      | 4 -> Some "BB"
      | _ -> None
    let size d = 4
    let reset d = ()
  end

  module D = Decoder (Dict_stub)

  let name = "decode"

  let _ =
    "TEST : " ^ name |> print_endline;

    let dict = Dict_stub.make () in
    let decode = D.decode
      ~find_opt:(Dict_stub.find_opt dict)
      ~size:(fun _ -> Dict_stub.size dict)
    in
      ( match decode { seq = "" ; code = 1 } with
      | D.Change.First seq ->
        assert (seq = "A")
      | _ -> assert false );

      ( match decode { seq = "A" ; code = 1 } with
      | D.Change.Found { emit_seq ; dict_seq } ->
        assert (emit_seq = "A") ;
        assert (dict_seq = "AA")
      | _ -> assert false );

      ( match decode { seq = "A" ; code = 2 } with
      | D.Change.Found { emit_seq ; dict_seq } ->
        assert (emit_seq = "B") ;
        assert (dict_seq = "AB")
      | _ -> assert false );

      ( match decode { seq = "B" ; code = 1 } with
      | D.Change.Found { emit_seq ; dict_seq } ->
        assert (emit_seq = "A") ;
        assert (dict_seq = "BA")
      | _ -> assert false );

      ( match decode { seq = "B" ; code = 2 } with
      | D.Change.Found { emit_seq ; dict_seq } ->
        assert (emit_seq = "B") ;
        assert (dict_seq = "BB")
      | _ -> assert false );

      ( match decode { seq = "AB" ; code = 3 } with
      | D.Change.Found { emit_seq ; dict_seq } ->
        assert (emit_seq = "AB") ;
        assert (dict_seq = "ABA")
      | _ -> assert false );

      ( match decode { seq = "A" ; code = 4 } with
      | D.Change.Found { emit_seq ; dict_seq } ->
        assert (emit_seq = "BB") ;
        assert (dict_seq = "AB")
      | _ -> assert false );

      ( match decode { seq = "B" ; code = 4 } with
      | D.Change.Found { emit_seq ; dict_seq } ->
        assert (emit_seq = "BB") ;
        assert (dict_seq = "BB")
      | _ -> assert false );

      ( match decode { seq = "" ; code = 42 } with
      | D.Change.Reset -> ()
      | _ -> assert false );

      ( match decode { seq = "A" ; code = 42 } with
      | D.Change.Reset -> ()
      | _ -> assert false );

      ( match decode { seq = "B" ; code = 42 } with
      | D.Change.Reset -> ()
      | _ -> assert false );

      ( match decode { seq = "AB" ; code = 42 } with
      | D.Change.Reset -> ()
      | _ -> assert false );

      ( match decode { seq = "BB" ; code = 42 } with
      | D.Change.Reset -> ()
      | _ -> assert false );

    "PASS : " ^ name |> print_endline

end

(* ================================================================ *)

module type Code_decoder = sig
  val make : unit -> int -> string list
end

module Decode (Dec : Code_decoder) = struct

  let list_to_string xs =
    let tokens = ref [] in
    let push t = tokens := t :: !tokens in
    let push_all = List.iter push in
    let decode = Dec.make () in
    let f c = c |> decode |> push_all in
    List.iter f xs ;
    List.rev !tokens |> String.concat ""

end

(* ================================================================ *)

module type Decode_conf = sig
  val capacity : int
  val alphabet : string
end

module Decode_dict (Conf : Decode_conf) = struct

  let _ =
    let cap = Conf.capacity in
    let alpha_len = String.length Conf.alphabet in
    assert (cap > 0) ;
    assert (alpha_len <= cap) ;
    let module CS = Set.Make (Char) in
    let unique = Conf.alphabet |> String.to_seq |> CS.of_seq in
    assert (alpha_len = CS.cardinal unique)

  type t = (int, string) Hashtbl.t

  let reset_code = Conf.capacity + 1

  let reset dict =
    Hashtbl.clear dict ;
    let map (i, c) = (i+1, String.make 1 c) in
    Conf.alphabet
    |> String.to_seqi
    |> Seq.map map
    |> Hashtbl.add_seq dict

  let make () =
    let dict = Hashtbl.create Conf.capacity in
    reset dict ;
    dict

  let size = Hashtbl.length

  exception Capacity_overflow

  let add dict key =
    (* if size dict = Conf.capacity then raise Capacity_overflow ; *)
    Hashtbl.add dict (Hashtbl.length dict + 1) key

  let find_opt = Hashtbl.find_opt

end

(* ================================================================ *)

module Decode_test = struct

  let _ =
    let module Dict = Decode_dict (struct
      let capacity = 42
      let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    end) in

    let module Decode = Decode (Decoder (Dict)) in

    let name = "list_to_string" in
    "TEST : " ^ name |> print_endline ;

    let input =
      [ 20; 15; 2; 5; 15; 18; 14; 15
      ; 20; 27; 29; 31; 36; 30; 32; 34 ]
    in
    let expected = "TOBEORNOTTOBEORTOBEORNOT" in
    let actual = Decode.list_to_string input in
    if actual = expected then
      "PASS : " ^ name |> print_endline
    else begin
      "FAIL : " ^ name |> print_endline ;
      "    expected: " ^ expected |> print_endline ;
      "    actual:   " ^ actual |> print_endline
    end

  let _ =
    let module Dict = Decode_dict (struct
      let capacity = 26
      let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    end) in

    let module Decode = Decode (Decoder (Dict)) in

    let name = "string_to_list (with overflow)" in
    "TEST : " ^ name |> print_endline ;

    let input = [20; 27; 15; 27; 2; 27; 5; 27; 20; 27; 15; 27] in
    let expected = "TOBETO" in
    let actual = Decode.list_to_string input in
    if actual = expected then
      "PASS : " ^ name |> print_endline
    else begin
      "FAIL : " ^ name |> print_endline ;
      "    expected: " ^ expected |> print_endline ;
      "    actual:   " ^ actual |> print_endline
    end

end

(* ================================================================ *)

module Decode_demo = struct

  module Dict = Decode_dict (struct
    let capacity = 42
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  end)

  module Decode = Decode (Decoder (Dict))

  let run input = input
    |> Decode.list_to_string
    |> print_endline

  let _ =
    run [ 20; 15; 27 ] ;
    run [ 20; 15; 2; 5; 15; 18; 14; 15; 20; 27; 29; 31; 36; 30; 32; 34 ] ;
    print_endline "TOBEORNOTTOBEORTOBEORNOT"

end

(* ================================================================ *)

module type Full_conf = sig
  val capacity : int
  val alphabet : string
end

module Full_coder (Conf : Full_conf) = struct

  module Encode = Encode (Encoder (Encode_dict (Conf)))
  module Decode = Decode (Decoder (Decode_dict (Conf)))

  type decoded = string
  type encoded = int list

  let encode = Encode.string_to_list
  let decode = Decode.list_to_string

end

(* ================================================================ *)

module type Coder = sig
  type decoded = string
  type encoded

  val encode : decoded -> encoded
  val decode : encoded -> decoded
end

module Full_test (C : Coder) = struct

  let test input =
    let result = input |> C.encode |> C.decode in
    assert (result = input)

  let name = "full"

  let _ =
    "TEST : " ^ name |> print_endline ;
    test "TOTO" ;
    test "TOBEOR" ;
    test "TOBEORTOBEER" ;
    test "TOBEORNOTTOBE" ;
    test "TWOBEERORNOTTWOBEER" ;
    test "TOBEORNOTTOBEORTOBEORNOT" ;
    "PASS : " ^ name |> print_endline

end

module Test = struct

  module Conf = struct
    let capacity = 42
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  end

  module Coder = Full_coder (Conf)

  module T = Full_test (Coder)

end