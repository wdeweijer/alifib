(** {1 Diagrams} *)

(** {2 Core types} *)
type t = { shape: Ogposet.t; labels: Id.tag array array }

val shape : t -> Ogposet.t
val labels : t -> Id.tag array array
val dim : t -> int
val is_round : t -> bool

(** {2 Error-handling} *)
type error = { message: string; notes: string list }

val error : ?notes:string list -> string -> error
val pp_error : Format.formatter -> error -> unit

type 'a checked = ('a, error) result

(** {2 Constructors} *)
val cell0 : Id.tag -> t checked

val cellN : Id.tag -> t -> t -> t checked
