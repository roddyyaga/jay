type ('terminal, 'nonterminal) completion =
  (('terminal, 'nonterminal) Grammar.rule * int) * int * int
[@@deriving show]

type ('t, 'nt) finished_node = ('t, 'nt) completion * ('t, 'nt) child list

and ('t, 'nt) child = Nt of ('t, 'nt) finished_node | T of 't
[@@deriving show]

let npad s n = List.init n (fun _ -> s) |> String.concat ""

let print_tree string_of_nt string_of_t node =
  let rec inner depth (((rule, _), origin, k), children) =
    let padding = npad "  " depth in
    let rule_string = Grammar.string_of_rule string_of_nt rule in
    Printf.printf "%s%s [%d, %d]\n" padding rule_string origin k;
    List.iter
      (function
        | Nt node -> inner (depth + 1) node
        | T t -> Printf.printf "%s%s\n" padding (string_of_t t))
      children
  in
  inner 0 node

type ('t, 'nt) node = {
  item: ('t, 'nt) completion;
  finished_children: ('t, 'nt) child list;
  remaining_children: ('t, 'nt) Grammar.symbol list;
  current_index: int;
}
[@@deriving show]

let completions_at completions origin max_k nonterminal =
  completions.(origin)
  |> ListLabels.filter ~f:(fun (((lhs, _rhs), _), k) ->
         lhs = nonterminal && k <= max_k)

let build_all completions tokens =
  let open Grammar in
  let (((_lhs, start_rhs), _) as keyed_rule), k =
    let initial_items = completions.(0) in
    match
      ListLabels.find_opt initial_items ~f:(fun (_rule, k) ->
          k = List.length tokens)
    with
    | None -> failwith "No completions from start to end found"
    | Some item -> item
  in
  let rec search
      ({ item; finished_children; remaining_children; current_index } as _node)
      =
    match remaining_children with
    | [] ->
        let _rule, _origin, k = item in
        if current_index = k then [ (item, finished_children) ] else []
    | next :: remaining' -> (
        match next with
        | Terminal terminal_predicate -> (
            match List.nth_opt tokens current_index with
            | Some t when terminal_predicate t ->
                search
                  {
                    item;
                    finished_children;
                    remaining_children = remaining';
                    current_index = current_index + 1;
                  }
            | _ -> [] )
        | Nonterminal nt ->
            let this_rule, origin, k = item in
            let candidates = completions_at completions current_index k nt in
            candidates
            |> List.filter (fun (rule, k') ->
                   not
                     ( snd this_rule = snd rule
                     && origin = current_index && k = k' ))
            |> List.concat_map (fun ((((_lhs, rhs), _) as rule'), k') ->
                   let item' = (rule', current_index, k') in
                   let finished_children' = [] in
                   let remaining_children' = rhs in
                   let current_index' = current_index in
                   search
                     {
                       item = item';
                       finished_children = finished_children';
                       remaining_children = remaining_children';
                       current_index = current_index';
                     })
            |> List.concat_map
                 (fun (((_rule, _origin, k'), _children) as new_finished_child)
                 ->
                   let finished_children' =
                     finished_children @ [ Nt new_finished_child ]
                   in
                   let current_index' = k' in
                   search
                     {
                       item;
                       finished_children = finished_children';
                       remaining_children = remaining';
                       current_index = current_index';
                     }) )
  in
  search
    {
      item = (keyed_rule, 0, k);
      finished_children = [];
      remaining_children = start_rhs;
      current_index = 0;
    }

let list_first_some f xs =
  List.fold_left
    (fun accum x -> match accum with Some y -> Some y | None -> f x)
    None xs

(* TODO - combine with build_all *)
let build_one completions tokens =
  let open Grammar in
  let (((_lhs, start_rhs), _) as keyed_rule), k =
    let initial_items = completions.(0) in
    match
      ListLabels.find_opt initial_items ~f:(fun (_rule, k) ->
          k = List.length tokens)
    with
    | None -> failwith "No completions from start to end found"
    | Some item -> item
  in
  let rec search
      ({ item; finished_children; remaining_children; current_index } as _node)
      =
    match remaining_children with
    | [] ->
        let _rule, _origin, k = item in
        if current_index = k then Some (item, finished_children) else None
    | next :: remaining' -> (
        match next with
        | Terminal terminal_predicate -> (
            match List.nth_opt tokens current_index with
            | Some t when terminal_predicate t ->
                search
                  {
                    item;
                    finished_children = finished_children @ [ T t ];
                    remaining_children = remaining';
                    current_index = current_index + 1;
                  }
            | _ -> None )
        | Nonterminal nt ->
            let this_rule, origin, k = item in
            let candidates = completions_at completions current_index k nt in
            candidates
            |> List.filter (fun (rule, k') ->
                   not
                     ( snd this_rule = snd rule
                     && origin = current_index && k = k' ))
            |> list_first_some (fun ((((_lhs, rhs), _) as rule'), k') ->
                   let item' = (rule', current_index, k') in
                   let finished_children' = [] in
                   let remaining_children' = rhs in
                   let current_index' = current_index in
                   let finished_child_opt =
                     search
                       {
                         item = item';
                         finished_children = finished_children';
                         remaining_children = remaining_children';
                         current_index = current_index';
                       }
                   in
                   match finished_child_opt with
                   | None -> None
                   | Some
                       (((_rule, _origin, k'), _children) as new_finished_child)
                     ->
                       let finished_children' =
                         finished_children @ [ Nt new_finished_child ]
                       in
                       let current_index' = k' in
                       search
                         {
                           item;
                           finished_children = finished_children';
                           remaining_children = remaining';
                           current_index = current_index';
                         }) )
  in
  search
    {
      item = (keyed_rule, 0, k);
      finished_children = [];
      remaining_children = start_rhs;
      current_index = 0;
    }
