type t

val domain_of_definition : t -> Id.Tag.t list
val is_defined_at : t -> Id.Tag.t -> bool
val image : t -> Id.Tag.t -> Diagram.t Error.checked
