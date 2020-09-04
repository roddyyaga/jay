val find_or_add :
  ('key, 'value) Hashtbl.t -> 'key -> default:(unit -> 'value) -> 'value
