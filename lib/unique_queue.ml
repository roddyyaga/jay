module Make (Key : Hashtbl.HashedType) = struct
  module Tbl = Hashtbl.Make (Key)

  type set = unit Tbl.t

  type t = Key.t Queue.t * set

  let create () = (Queue.create (), Tbl.create 16)

  let is_empty (queue, _sets) = Queue.is_empty queue

  let push x (queue, set) =
    let check = Tbl.mem set x in
    if check then false
    else (
      Queue.push x queue;
      Tbl.add set x ();
      true )

  let pop (queue, _) = Queue.take_opt queue

  let pop_exn (queue, _) = Queue.take queue
end
