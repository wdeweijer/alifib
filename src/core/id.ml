type error = Error.t
type 'a checked = 'a Error.checked

module Global = struct
  type t = int

  let counter = ref 0

  let fresh () =
    let i = !counter in
    incr counter ; i

  let equal = Int.equal
  let compare = Int.compare
  let hash = Hashtbl.hash
  let to_int x = x
  let pp fmt x = Format.fprintf fmt "#%d" x
end

module Local = struct
  type simple = string
  type t = string

  let reserved = [ "include"; "attach"; "Type"; "along" ]

  let is_valid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
        true
    | _ ->
        false

  let check_simple s =
    if String.length s = 0 then Error (Error.make "name must not be empty")
    else if List.exists (String.equal s) reserved then
      Error (Error.make ("name is reserved: " ^ s))
    else if String.for_all is_valid_char s then Ok s
    else Error (Error.make "name contains invalid characters")

  let simple s = check_simple s
  let simple_to_string s = s
  let string_is_empty s = String.length s = 0

  let check_name s =
    if String.length s = 0 then Error (Error.make "name must not be empty")
    else
      let parts = String.split_on_char '.' s in
      if List.exists string_is_empty parts then
        Error (Error.make "name must not contain empty segments")
      else
        let rec validate acc = function
          | [] ->
              Ok (List.rev acc)
          | part :: rest -> (
              match check_simple part with
              | Ok p ->
                  validate (p :: acc) rest
              | Error _ as e ->
                  e)
        in
        match validate [] parts with Ok _ -> Ok s | Error err -> Error err

  let make s = check_name s
  let to_string n = n
end

module Module = struct
  type t = string

  let of_path path = path
  let to_string id = id
  let equal = String.equal
  let compare = String.compare
  let hash = Hashtbl.hash
  let pp fmt id = Format.fprintf fmt "%s" id
end

module Tag = struct
  type t = [ `Local of Local.t | `Global of Global.t ]

  let equal a b =
    match (a, b) with
    | `Local x, `Local y ->
        String.equal x y
    | `Global x, `Global y ->
        Global.equal x y
    | (`Local _ | `Global _), _ ->
        false

  let compare a b =
    match (a, b) with
    | `Local x, `Local y ->
        String.compare x y
    | `Global x, `Global y ->
        Global.compare x y
    | `Local _, `Global _ ->
        -1
    | `Global _, `Local _ ->
        1

  let pp fmt = function
    | `Local name ->
        Format.fprintf fmt "%s" (Local.to_string name)
    | `Global id ->
        Global.pp fmt id

  let of_local name = `Local name
  let of_global id = `Global id
end
