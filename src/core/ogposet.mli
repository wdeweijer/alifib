(** {1 Oriented graded posets (ogposets)}

    Minimal internal data structure with embeddings and standard
    categorical constructions. No well-formedness checks are performed;
    this module is designed for use inside verified higher-level code. *)

(** {2 Core types} *)

type t
type poset = t
(** An oriented graded poset (ogposet). *)

type intset
(** Abstract type of sets of integers used for faces and cofaces. *)

type sign = [ `Input | `Output | `Both ]
(** Orientation: input, output, or both. *)

val make :
  dim:int ->
  faces_in:  intset array array ->
  faces_out: intset array array ->
  cofaces_in:  intset array array ->
  cofaces_out: intset array array ->
  t
(** Build an ogposet directly from adjacency arrays of integer sets and its dimension. *)

val dim   : t -> int
val sizes : t -> int array

(** {2 Local structure} *)

val faces_of   : sign -> t -> dim:int -> pos:int -> intset
val cofaces_of : sign -> t -> dim:int -> pos:int -> intset
(** Access the input/output/both faces or cofaces of a given element. *)

(** {2 Embeddings (morphisms of ogposets)} *)

module Embedding : sig
  type t
  val make    : dom:poset -> cod:poset -> map:int array array -> t
  val dom     : t -> poset
  val cod     : t -> poset
  val map     : t -> int array array
  val id      : poset -> t
  val compose : t -> t -> t
end

(** {2 Universal constructions} *)

type coproduct = { sum : t; inl : Embedding.t; inr : Embedding.t }
val coproduct : t -> t -> coproduct

type pushout = { po : t; leg1 : Embedding.t; leg2 : Embedding.t }
val pushout : Embedding.t -> Embedding.t -> pushout

type coequaliser = { coeq : t; emb : Embedding.t }
val coequaliser : Embedding.t -> Embedding.t -> coequaliser

(** {2 Derived substructures} *)

val boundary : sign -> int -> t -> t * Embedding.t
(** [boundary sign at_dim X] is the induced sub-ogposet on the downward
    closure of those [at_dim]-cells of [X] having no cofaces of the given
    orientation, together with its inclusion embedding. *)

val boundary_top : sign -> t -> t * Embedding.t
(** [boundary_top sign X] = [boundary sign (dim X - 1) X]. *)