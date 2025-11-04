module GlobalOrd = struct
  type t = Id.Global.t

  let compare : t -> t -> int = Id.Global.compare
end

module ModuleOrd = struct
  type t = Id.Module.t

  let compare : t -> t -> int = Id.Module.compare
end

module GlobalMap = Map.Make (GlobalOrd)
module ModuleMap = Map.Make (ModuleOrd)

type type_entry = { data: Diagram.cell_data; complex: Complex.t }

type t = {
  cells: Diagram.cell_data GlobalMap.t;
  types: type_entry GlobalMap.t;
  modules: Complex.t ModuleMap.t;
}

let empty =
  { cells= GlobalMap.empty; types= GlobalMap.empty; modules= ModuleMap.empty }

let add_cell state ~id data =
  { state with cells= GlobalMap.add id data state.cells }

let add_type state ~id ~data ~complex =
  { state with types= GlobalMap.add id { data; complex } state.types }

let update_type_complex state ~id complex =
  let types =
    match GlobalMap.find_opt id state.types with
    | Some entry ->
        GlobalMap.add id { entry with complex } state.types
    | None ->
        state.types
  in
  { state with types }

let add_module state ~id complex =
  { state with modules= ModuleMap.add id complex state.modules }

let find_cell state id = GlobalMap.find_opt id state.cells
let find_type state id = GlobalMap.find_opt id state.types
let find_module state id = ModuleMap.find_opt id state.modules

let pp fmt state =
  let open Format in
  let module_entries = ModuleMap.bindings state.modules in
  let render_list items =
    match items with [] -> "(none)" | _ -> String.concat ", " items
  in
  let string_of_tag = function
    | `Local name ->
        Id.Local.to_string name
    | `Global id ->
        asprintf "%a" Id.Global.pp id
  in
  let pp_module fmt (module_id, module_complex) =
    fprintf fmt "@[<v>* Module %s" (Id.Module.to_string module_id)
    ; let generator_names = Complex.generator_names module_complex in
      let pp_type fmt generator_name =
        let type_label = Id.Local.to_string generator_name in
        let print_details cells diagrams morphisms =
          fprintf fmt
            "@[<v 2>Type %s@,- Cells: %s@,- Diagrams: %s@,- Morphisms: %s@]"
            type_label (render_list cells) (render_list diagrams)
            (render_list morphisms)
        in
        match Complex.find_generator module_complex generator_name with
        | None ->
            print_details [ "(missing generator)" ] [ "(missing generator)" ]
              [ "(missing generator)" ]
        | Some { tag; _ } -> (
            match tag with
            | `Local _ ->
                print_details [ "(local tag)" ] [ "(local tag)" ]
                  [ "(local tag)" ]
            | `Global global_id -> (
                match find_type state global_id with
                | None ->
                    print_details [ "(type not found)" ] [ "(type not found)" ]
                      [ "(type not found)" ]
                | Some { complex= type_complex; _ } ->
                    let cells =
                      Complex.generator_names type_complex
                      |> List.map Id.Local.to_string
                    in
                    let diagrams =
                      Complex.diagram_names type_complex
                      |> List.map Id.Local.to_string
                    in
                    let morphisms =
                      Complex.morphism_names type_complex
                      |> List.map (fun morph_name ->
                             let domain_label =
                               match
                                 Complex.find_morphism type_complex morph_name
                               with
                               | Some { domain; _ } -> (
                                   match
                                     Complex.find_generator_by_tag
                                       module_complex domain
                                   with
                                   | Some domain_name ->
                                       Id.Local.to_string domain_name
                                   | None ->
                                       string_of_tag domain)
                               | None ->
                                   "?"
                             in
                             Printf.sprintf "%s :: %s"
                               (Id.Local.to_string morph_name)
                               domain_label)
                    in
                    print_details cells diagrams morphisms))
      in
      if generator_names <> [] then (
        fprintf fmt "@,"
        ; pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt "@,@,")
            (fun fmt name -> pp_type fmt name)
            fmt generator_names)
      ; fprintf fmt "@]"
  in
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@,@,")
    pp_module fmt module_entries
