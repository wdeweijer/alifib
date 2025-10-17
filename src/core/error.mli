type t = { message: string; notes: string list }

val make : ?notes:string list -> string -> t
val pp : Format.formatter -> t -> unit

type 'a checked = ('a, t) result
