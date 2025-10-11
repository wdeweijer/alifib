(* Minimal oriented graded posets with embeddings. Internal: no well-formedness
   checks. Efficient and replaceable IntSet backend. *)

(* --- Replaceable integer set implementation --------------------------- *)

module IntSet = Set.Make (Int)
(* Swap this definition if desired; only this module needs to change. *)

type intset = IntSet.t

(* --- Core definitions ------------------------------------------------- *)

type sign = [ `Input | `Output | `Both ]
type grade = IntSet.t array
type adjacency = grade array

type t = {
  dim: int; (* stored for efficiency *)
  faces_in: adjacency;
  faces_out: adjacency;
  cofaces_in: adjacency;
  cofaces_out: adjacency;
}

type poset = t

let dim x = x.dim
let sizes x = Array.init (x.dim + 1) (fun d -> Array.length x.faces_in.(d))

let make ~dim ~faces_in ~faces_out ~cofaces_in ~cofaces_out =
  { dim; faces_in; faces_out; cofaces_in; cofaces_out }

let empty =
  {
    dim= -1;
    faces_in= [||];
    faces_out= [||];
    cofaces_in= [||];
    cofaces_out= [||];
  }

let point =
  let make_level () = Array.make 1 IntSet.empty in
  {
    dim= 0;
    faces_in= [| make_level () |];
    faces_out= [| make_level () |];
    cofaces_in= [| make_level () |];
    cofaces_out= [| make_level () |];
  }

(* --- Accessors -------------------------------------------------------- *)

let faces_of sign x ~dim ~pos =
  match sign with
  | `Input ->
      x.faces_in.(dim).(pos)
  | `Output ->
      x.faces_out.(dim).(pos)
  | `Both ->
      IntSet.union x.faces_in.(dim).(pos) x.faces_out.(dim).(pos)

let cofaces_of sign x ~dim ~pos =
  match sign with
  | `Input ->
      x.cofaces_in.(dim).(pos)
  | `Output ->
      x.cofaces_out.(dim).(pos)
  | `Both ->
      IntSet.union x.cofaces_in.(dim).(pos) x.cofaces_out.(dim).(pos)

(* --- Embeddings ------------------------------------------------------- *)

module Embedding = struct
  type t = { dom: poset; cod: poset; map: int array array }

  let make ~dom ~cod ~map = { dom; cod; map }
  let dom e = e.dom
  let cod e = e.cod
  let map e = e.map
  let empty cod = { dom= empty; cod; map= [||] }

  let id x =
    let sz = sizes x in
    let map = Array.map (fun n -> Array.init n (fun i -> i)) sz in
    { dom= x; cod= x; map }

  let compose f g =
    let map =
      Array.mapi
        (fun d arrf ->
          Array.init (Array.length arrf) (fun i ->
              let mid = arrf.(i) in
              g.map.(d).(mid)))
        f.map
    in
    { dom= f.dom; cod= g.cod; map }
end

(* --- Utility helpers -------------------------------------------------- *)

let set_map f s = IntSet.fold (fun x acc -> IntSet.add (f x) acc) s IntSet.empty

let set_filter_map f s =
  IntSet.fold
    (fun x acc -> match f x with None -> acc | Some y -> IntSet.add y acc)
    s IntSet.empty

(* --- Boundary --------------------------------------------------------- *)

let extremal (s : sign) (k : int) (g : t) : intset =
  let n = Array.length g.faces_in.(k) in
  match s with
  | `Input ->
      let acc = ref IntSet.empty in
      for i = 0 to n - 1 do
        if IntSet.is_empty (cofaces_of `Output g ~dim:k ~pos:i) then
          acc := IntSet.add i !acc
      done
      ; !acc
  | `Output ->
      let acc = ref IntSet.empty in
      for i = 0 to n - 1 do
        if IntSet.is_empty (cofaces_of `Input g ~dim:k ~pos:i) then
          acc := IntSet.add i !acc
      done
      ; !acc
  | `Both ->
      let acc = ref IntSet.empty in
      for i = 0 to n - 1 do
        if
          IntSet.is_empty (cofaces_of `Input g ~dim:k ~pos:i)
          || IntSet.is_empty (cofaces_of `Output g ~dim:k ~pos:i)
        then acc := IntSet.add i !acc
      done
      ; !acc

let maximal (k : int) (g : t) : intset =
  let n = Array.length g.faces_in.(k) in
  let acc = ref IntSet.empty in
  for i = 0 to n - 1 do
    if IntSet.is_empty (cofaces_of `Both g ~dim:k ~pos:i) then
      acc := IntSet.add i !acc
  done
  ; !acc

let is_pure (g : t) : bool =
  let n = g.dim in
  if n <= 1 then true
  else
    let rec aux k =
      if k >= n then true else IntSet.is_empty (maximal k g) && aux (k + 1)
    in
    aux 0

let is_round (g : t) : bool =
  let n = g.dim in
  if n <= 1 then true
  else if not (is_pure g) then false
  else
    (* Propagate input/output interior cells dimension by dimension, while
       avoiding cells already claimed at the previous level. *)
    let in_interior = Array.make n [||] in
    let out_interior = Array.make n [||] in
    let rec step j =
      if j >= n then true
      else
        let build_layer base =
          let layer = Array.init (j + 1) (fun _ -> IntSet.empty) in
          layer.(j) <- base
          ; let prev_in = if j = 0 then None else Some in_interior.(j - 1)
            and prev_out = if j = 0 then None else Some out_interior.(j - 1) in
            for i = j - 1 downto 0 do
              let prev_in_i =
                match prev_in with None -> IntSet.empty | Some arr -> arr.(i)
              and prev_out_i =
                match prev_out with None -> IntSet.empty | Some arr -> arr.(i)
              in
              IntSet.iter
                (fun p ->
                  let faces = faces_of `Both g ~dim:(i + 1) ~pos:p in
                  IntSet.iter
                    (fun q ->
                      if
                        (not (IntSet.mem q prev_in_i))
                        && not (IntSet.mem q prev_out_i)
                      then layer.(i) <- IntSet.add q layer.(i))
                    faces)
                layer.(i + 1)
            done
            ; layer
        in
        let layer_in = build_layer (extremal `Input j g)
        and layer_out = build_layer (extremal `Output j g) in
        let rec has_overlap i =
          if i < 0 then false
          else
            let overlap = IntSet.inter layer_in.(i) layer_out.(i) in
            if IntSet.is_empty overlap then has_overlap (i - 1) else true
        in
        if has_overlap j then false
        else (
          in_interior.(j) <- layer_in
          ; out_interior.(j) <- layer_out
          ; step (j + 1))
    in
    step 0

type embedding_data = {
  forward: int array array; (* boundary index -> original index in g *)
  inverse: int array array; (* original index in g -> boundary index, or -1 *)
}

let boundary (s : sign) (k : int) (g : t) : t * Embedding.t =
  if k >= g.dim then (g, Embedding.id g)
  else if k < 0 then (empty, Embedding.empty g)
  else
    let dims_b = k + 1 in
    let sz_g = sizes g in

    (* Accumulators: per-dimension lists in insertion order (we’ll reverse
       once). *)
    let acc : int list array = Array.init dims_b (fun _ -> []) in
    let inv : int array array =
      Array.init dims_b (fun j -> Array.make sz_g.(j) (-1))
    in
    let next_new_index : int array = Array.make dims_b 0 in

    (* Insert into F at level j; also update inv and next_new_index. *)
    let insert_F (j : int) (old : int) =
      let i = next_new_index.(j) in
      inv.(j).(old) <- i
      ; acc.(j) <- old :: acc.(j)
      ; next_new_index.(j) <- i + 1
    in

    (* Step 1: level k — add extremal s k g, no membership check needed *)
    IntSet.iter (fun i -> insert_F k i) (extremal s k g)

    ; (* Step 2: recursively fill lower levels *)
      for j = k - 1 downto 0 do
        (* (1) faces of (j+1)-cells already in F: need membership checks via
           inv *)
        let parents = List.rev acc.(j + 1) in
        (* reverse to preserve insertion order *)
        List.iter
          (fun parent_old ->
            IntSet.iter
              (fun f -> if inv.(j).(f) < 0 then insert_F j f)
              (faces_of `Both g ~dim:(j + 1) ~pos:parent_old))
          parents
        ; (* (2) maximal j-cells: no checks needed (cannot be faces of
             (j+1)-cells) *)
          IntSet.iter (fun m -> insert_F j m) (maximal j g)
      done

    ; (* Finalize forward map by reversing accumulators to restore insertion
         order. *)
      let forward : int array array =
        Array.init dims_b (fun j -> acc.(j) |> List.rev |> Array.of_list)
      in

      let ed : embedding_data = { forward; inverse= inv } in

      (* Local helper: remap an adjacency family using the already-built maps. -
         shift = -1 → faces (use set_map; all targets are guaranteed present) -
         shift = +1 → cofaces (use set_filter_map; some targets may be
         absent) *)
      let build_adj (adj : adjacency) (shift : int) : adjacency =
        let empty = IntSet.empty in
        Array.init dims_b (fun j ->
            let nj = Array.length ed.forward.(j) in
            if (shift = -1 && j = 0) || (shift = 1 && j = dims_b - 1) then
              Array.make nj empty
            else
              Array.init nj (fun i ->
                  let old = ed.forward.(j).(i) in
                  let target_dim = j + shift in
                  if shift = -1 then
                    (* faces: every referenced face must be in the boundary by
                       construction *)
                    set_map (fun x -> ed.inverse.(target_dim).(x)) adj.(j).(old)
                  else
                    (* cofaces: may reference cells outside the boundary; filter
                       those out *)
                    set_filter_map
                      (fun x ->
                        let y = ed.inverse.(target_dim).(x) in
                        if y < 0 then None else Some y)
                      adj.(j).(old)))
      in

      (* Build boundary adjacencies using the local helper. *)
      let faces_in' = build_adj g.faces_in (-1)
      and faces_out' = build_adj g.faces_out (-1)
      and cofaces_in' = build_adj g.cofaces_in 1
      and cofaces_out' = build_adj g.cofaces_out 1 in

      (* Assemble boundary ogposet and inclusion embedding *)
      let sub =
        {
          dim= k;
          faces_in= faces_in';
          faces_out= faces_out';
          cofaces_in= cofaces_in';
          cofaces_out= cofaces_out';
        }
      in
      let emb = Embedding.make ~dom:sub ~cod:g ~map:ed.forward in
      (sub, emb)
