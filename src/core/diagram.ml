type t = { shape: Ogposet.t; labels: Id.tag array array }
type error = { message: string; notes: string list }

let error ?(notes = []) message = { message; notes }

let pp_error fmt { message; notes } =
  let open Format in
  fprintf fmt "%s" message
  ; match notes with
    | [] ->
        ()
    | _ ->
        List.iter (fun note -> fprintf fmt "@.@[<2>note:@ %s@]" note) notes

type 'a checked = ('a, error) result

let shape d = d.shape
let labels d = d.labels
let dim d = Ogposet.dim d.shape
let is_round d = Ogposet.is_round d.shape

let cell0 tag =
  let shape = Ogposet.point in
  let labels = [| [| tag |] |] in
  Ok { shape; labels }

let pullback d emb =
  let dom = Ogposet.Embedding.dom emb in
  let cod_labels = labels d in
  let map = Ogposet.Embedding.map emb in
  let pulled =
    Array.mapi
      (fun dim level_map ->
        Array.map (fun idx -> cod_labels.(dim).(idx)) level_map)
      map
  in
  { shape= dom; labels= pulled }

let labels_equal a b =
  let len = Array.length a in
  len = Array.length b
  &&
  let rec dim_loop i =
    if i = len then true
    else
      let row_a = a.(i) and row_b = b.(i) in
      let row_len = Array.length row_a in
      row_len = Array.length row_b
      &&
      let rec cell_loop j =
        if j = row_len then true
        else if row_a.(j) = row_b.(j) then cell_loop (j + 1)
        else false
      in
      cell_loop 0 && dim_loop (i + 1)
  in
  dim_loop 0

let build_stack_cellN shape =
  let d = Ogposet.dim shape in
  let rec gather acc k =
    if k < 0 then acc
    else
      let set = Ogposet.extremal `Input k shape in
      gather ((k, set) :: acc) (k - 1)
  in
  let inputs = gather [] (d - 1) in
  if d > 0 then inputs @ [ (d - 1, Ogposet.extremal `Output (d - 1) shape) ]
  else inputs

let build_stack_paste sign shape max_dim =
  let rec aux acc k =
    if k < 0 then acc
    else aux ((k, Ogposet.extremal sign k shape) :: acc) (k - 1)
  in
  aux [] max_dim |> List.rev

let cellN tag u v =
  let shape_u = shape u and shape_v = shape v in
  let dim_u = Ogposet.dim shape_u and dim_v = Ogposet.dim shape_v in
  if dim_u <> dim_v then Error (error "dimensions do not match")
  else if not (is_round u) then Error (error "u is not round")
  else if not (is_round v) then Error (error "v is not round")
  else
    let d = dim_u in
    let bd_u, e_u = Ogposet.traverse shape_u (build_stack_cellN shape_u) in
    let bd_v, e_v = Ogposet.traverse shape_v (build_stack_cellN shape_v) in
    if not (Ogposet.equal bd_u bd_v) then
      Error (error "shapes of boundaries do not match")
    else
      let pb_u = pullback u e_u and pb_v = pullback v e_v in
      if not (labels_equal pb_u.labels pb_v.labels) then
        Error (error "boundaries do not match")
      else
        let { Ogposet.tip= bd_uv; inl; inr } = Ogposet.pushout e_u e_v in
        let sizes_bd = Ogposet.sizes bd_uv in
        let inl_map = Ogposet.Embedding.map inl in
        let inr_map = Ogposet.Embedding.map inr in
        let inl_inv = Ogposet.Embedding.inv inl in
        let inr_inv = Ogposet.Embedding.inv inr in
        let inl_map_d = inl_map.(d) in
        let inr_map_d = inr_map.(d) in
        let inl_inv_d = inl_inv.(d) in
        let inr_inv_d = inr_inv.(d) in
        let faces_in =
          Array.init (d + 2) (fun dim ->
              if dim <= d then
                Array.init sizes_bd.(dim) (fun pos ->
                    Ogposet.faces_of `Input bd_uv ~dim ~pos)
              else [| Ogposet.intset_of_array inl_map_d |])
        in
        let faces_out =
          Array.init (d + 2) (fun dim ->
              if dim <= d then
                Array.init sizes_bd.(dim) (fun pos ->
                    Ogposet.faces_of `Output bd_uv ~dim ~pos)
              else [| Ogposet.intset_of_array inr_map_d |])
        in
        let cofaces_in =
          Array.init (d + 2) (fun dim ->
              if dim < d then
                Array.init sizes_bd.(dim) (fun pos ->
                    Ogposet.cofaces_of `Input bd_uv ~dim ~pos)
              else if dim = d then
                Array.init (Array.length inl_inv_d) (fun idx ->
                    if inl_inv_d.(idx) >= 0 then Ogposet.intset_of_array [| 0 |]
                    else Ogposet.intset_empty)
              else [| Ogposet.intset_empty |])
        in
        let cofaces_out =
          Array.init (d + 2) (fun dim ->
              if dim < d then
                Array.init sizes_bd.(dim) (fun pos ->
                    Ogposet.cofaces_of `Output bd_uv ~dim ~pos)
              else if dim = d then
                Array.init (Array.length inr_inv_d) (fun idx ->
                    if inr_inv_d.(idx) >= 0 then Ogposet.intset_of_array [| 0 |]
                    else Ogposet.intset_empty)
              else [| Ogposet.intset_empty |])
        in
        let shape_uv =
          Ogposet.make ~dim:(d + 1) ~faces_in ~faces_out ~cofaces_in
            ~cofaces_out
        in
        let sizes_bd = Ogposet.sizes bd_uv in
        let labels_u = labels u and labels_v = labels v in
        let labels_base_opts =
          Array.init (d + 1) (fun dim -> Array.make sizes_bd.(dim) None)
        in
        Array.iteri
          (fun dim mapping ->
            Array.iteri
              (fun idx target ->
                labels_base_opts.(dim).(target) <- Some labels_u.(dim).(idx))
              mapping)
          inl_map
        ; Array.iteri
            (fun dim mapping ->
              Array.iteri
                (fun idx target ->
                  labels_base_opts.(dim).(target) <- Some labels_v.(dim).(idx))
                mapping)
            inr_map
        ; let labels_bd =
            Array.map
              (Array.map (function
                | Some tag_value ->
                    tag_value
                | None ->
                    assert false))
              labels_base_opts
          in
          let labels_uv =
            Array.init (d + 2) (fun dim ->
                if dim <= d then labels_bd.(dim) else [| tag |])
          in
          Ok { shape= shape_uv; labels= labels_uv }

let paste n u v =
  if n < 0 then Error (error "dimension of pasting must be positive")
  else
    let shape_u = shape u and shape_v = shape v in
    let dim_u = Ogposet.dim shape_u and dim_v = Ogposet.dim shape_v in
    let stack_u = build_stack_paste `Output shape_u (min n dim_u) in
    let stack_v = build_stack_paste `Input shape_v (min n dim_v) in
    let out_n_u, e_u = Ogposet.traverse shape_u stack_u in
    let in_n_v, e_v = Ogposet.traverse shape_v stack_v in
    if not (Ogposet.equal out_n_u in_n_v) then
      Error (error "shapes of boundaries do not match")
    else
      let pb_u = pullback u e_u and pb_v = pullback v e_v in
      if not (labels_equal pb_u.labels pb_v.labels) then
        Error (error "boundaries do not match")
      else
        let { Ogposet.tip= shape_uv; inl; inr } = Ogposet.pushout e_u e_v in
        let sizes_uv = Ogposet.sizes shape_uv in
        let labels_u = labels u and labels_v = labels v in
        let base_labels =
          Array.init (Array.length sizes_uv) (fun dim ->
              Array.make sizes_uv.(dim) None)
        in
        Array.iteri
          (fun dim mapping ->
            Array.iteri
              (fun idx target ->
                base_labels.(dim).(target) <- Some labels_u.(dim).(idx))
              mapping)
          (Ogposet.Embedding.map inl)
        ; Array.iteri
            (fun dim mapping ->
              Array.iteri
                (fun idx target ->
                  base_labels.(dim).(target) <- Some labels_v.(dim).(idx))
                mapping)
            (Ogposet.Embedding.map inr)
        ; let labels_uv =
            Array.map
              (Array.map (function
                | Some tag_value ->
                    tag_value
                | None ->
                    assert false))
              base_labels
          in
          Ok { shape= shape_uv; labels= labels_uv }

let boundary sign k d =
  let _, emb = Ogposet.boundary sign k d.shape in
  pullback d emb

let label_set_of d =
  let table = Hashtbl.create 16 in
  Array.iter
    (Array.iter (fun tag ->
         let count =
           match Hashtbl.find_opt table tag with Some c -> c + 1 | None -> 1
         in
         Hashtbl.replace table tag count))
    d.labels
  ; Hashtbl.fold (fun tag count acc -> (tag, count) :: acc) table []
    |> List.sort (fun (a, _) (b, _) -> Id.tag_compare a b)

let equal u v =
  if u == v then true
  else
    let shape_u = u.shape and shape_v = v.shape in
    let dim_u = Ogposet.dim shape_u and dim_v = Ogposet.dim shape_v in
    if dim_u <> dim_v then false
    else
      let n = dim_u in
      let sizes_u = Ogposet.sizes shape_u and sizes_v = Ogposet.sizes shape_v in
      if sizes_u <> sizes_v then false
      else if label_set_of u <> label_set_of v then false
      else if Ogposet.equal shape_u shape_v then labels_equal u.labels v.labels
      else
        let stack_u = build_stack_paste `Input shape_u n in
        let stack_v = build_stack_paste `Input shape_v n in
        let u', e_u = Ogposet.traverse shape_u stack_u in
        let v', e_v = Ogposet.traverse shape_v stack_v in
        if not (Ogposet.equal u' v') then false
        else
          let pb_u = pullback u e_u and pb_v = pullback v e_v in
          labels_equal pb_u.labels pb_v.labels
