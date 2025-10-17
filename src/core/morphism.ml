module TagTable = Hashtbl.Make (struct
  type t = Id.Tag.t

  let equal = Id.Tag.equal

  let hash = function
    | `Local name ->
        Hashtbl.hash (0, Id.Local.to_string name)
    | `Global id ->
        Hashtbl.hash (1, Id.Global.to_int id)
end)

type t = { table: Diagram.t TagTable.t }

let[@warning "-32"] of_list entries =
  (* Private helper: assumes entries already validated. *)
  let capacity = max 1 (List.length entries) in
  let table = TagTable.create capacity in
  List.iter
    (fun (tag, diagram) -> TagTable.replace table tag diagram)
    entries
  ; { table }

let domain_of_definition m =
  TagTable.fold (fun tag _ acc -> tag :: acc) m.table []

let is_defined_at m tag = TagTable.mem m.table tag

let image m tag =
  match TagTable.find_opt m.table tag with
  | Some diagram ->
      Ok diagram
  | None ->
      Error (Error.make "not in the domain of definition")
