let find_or_add hashtbl key ~default =
  match Hashtbl.find_opt hashtbl key with
  | Some value -> value
  | None ->
      let value = default () in
      Hashtbl.add hashtbl key value;
      value
