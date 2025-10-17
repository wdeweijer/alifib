type t = { message: string; notes: string list }

let make ?(notes = []) message = { message; notes }

let pp fmt { message; notes } =
  let open Format in
  fprintf fmt "%s" message
  ; match notes with
    | [] ->
        ()
    | _ ->
        List.iter (fun note -> fprintf fmt "@.@[<2>note:@ %s@]" note) notes

type 'a checked = ('a, t) result
