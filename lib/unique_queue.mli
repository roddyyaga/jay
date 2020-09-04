module Make (Key : Hashtbl.HashedType) : sig
  type t

  val create : unit -> t

  val is_empty : t -> bool

  val push : Key.t -> t -> bool

  val pop : t -> Key.t option

  val pop_exn : t -> Key.t
end
