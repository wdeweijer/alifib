type error = Error.t
type 'a checked = 'a Error.checked

module Global : sig
  type t

  val fresh : unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_int : t -> int
  val pp : Format.formatter -> t -> unit
end

module Local : sig
  type simple
  type t

  val simple : string -> simple checked
  val simple_to_string : simple -> string
  val make : string -> t checked
  val to_string : t -> string
end

module Tag : sig
  type t = [ `Local of Local.t | `Global of Global.t ]

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val of_local : Local.t -> t
  val of_global : Global.t -> t
end
