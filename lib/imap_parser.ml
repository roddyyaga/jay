type ('terminal, 'nonterminal) array_rule =
  'nonterminal * ('terminal, 'nonterminal) Grammar.symbol array

type ('terminal, 'nonterminal) completion =
  (('terminal, 'nonterminal) Grammar.rule * int) * int

let array_rule_to_list_rule (nt, arr) = (nt, Array.to_list arr)

let list_rule_to_array_rule (nt, xs) = (nt, Array.of_list xs)

type inner_loop_outcome = Loop_unfinished | Loop_finished

let recognise (type terminal nonterminal) ?token_to_string
    ( (top_level_rule : (terminal, nonterminal) Grammar.rule),
      (other_rules : (terminal, nonterminal) Grammar.rule list) ) tokens =
  (* Convert tokens to array for efficiency *)
  let tokens = Array.of_list tokens in

  (* Represent nonterminals as integers *)
  let nonterminal_encoder = Hashtbl.create 16 in
  let nonterminal_decoder = CCVector.create () in
  let nonterminal_count =
    ListLabels.fold_left
      (top_level_rule :: other_rules)
      ~f:(fun i (lhs, _) ->
        if not @@ Hashtbl.mem nonterminal_encoder lhs then (
          Hashtbl.add nonterminal_encoder lhs i;
          CCVector.push nonterminal_decoder lhs;
          i + 1 )
        else i)
      ~init:0
  in
  let encode_rule (lhs, rhs) =
    let open Grammar in
    ( Hashtbl.find nonterminal_encoder lhs,
      List.map
        (function
          | Terminal t -> Terminal t
          | Nonterminal nt -> Nonterminal (Hashtbl.find nonterminal_encoder nt))
        rhs )
  in
  let decode_rule (lhs, rhs) =
    let open Grammar in
    ( CCVector.get nonterminal_decoder lhs,
      List.map
        (function
          | Terminal t -> Terminal t
          | Nonterminal nt -> Nonterminal (CCVector.get nonterminal_decoder nt))
        rhs )
  in
  let top_level_rule = encode_rule top_level_rule in
  let other_rules = List.map encode_rule other_rules in

  (* Define the types for the queues of items to complete, predict and scan
     An item in one of these queues is a ((rule, dot_position, origin), next_state) tuple *)
  let module Keyed_rule = struct
    type nonrec terminal = terminal

    (* Give rules keys so they can be hashed (terminals may be functions) *)
    type t = { key: int; rule: (terminal, int) array_rule }
  end in
  let module Make_state (Type : sig
    type t
  end) =
  struct
    type nonrec t = (Keyed_rule.t * int * int) * Type.t

    let ( = ) ((({ Keyed_rule.key; _ }, i, j), _) : t)
        (({ Keyed_rule.key = key'; _ }, i', j'), _) =
      key = key' && i = i' && j = j'

    let equal = ( = )

    let hash (({ Keyed_rule.key; _ }, i, j), _) =
      CCHash.(combine3 (int key) (int i) (int j))
  end in
  let module Complete_state = Make_state (struct
    type t = unit
  end) in
  let module Predict_state = Make_state (struct
    type t = int
  end) in
  let module Scan_state = Make_state (struct
    type t = terminal -> bool
  end) in
  let rules =
    List.mapi
      (fun i rule ->
        { Keyed_rule.rule = list_rule_to_array_rule rule; key = i })
      (top_level_rule :: other_rules)
  in

  (* Store rules by their LHS nonterminal for efficiency in predicting *)
  let rules_by_lhs = Array.make nonterminal_count [] in
  ListLabels.iter rules ~f:(fun rule ->
      let lhs, _ = rule.Keyed_rule.rule in
      let set = rules_by_lhs.(lhs) in
      rules_by_lhs.(lhs) <- rule :: set);

  (* Create modules for the three queues *)
  let module Complete_queue = Unique_queue.Make (Complete_state) in
  (* The predict queue is different to the others because items are stored in an array
     indexed by their next nonterminal*)
  let module Predict_queue = struct
    module Tbl = Hashtbl.Make (Predict_state)

    type sets = unit Tbl.t option array

    type t = Predict_state.t Queue.t * sets

    let[@landmark "array_create"] create () : t =
      (Queue.create (), Array.make nonterminal_count None)

    let is_empty (queue, _) = Queue.is_empty queue

    let[@landmark "array_push"] push x ((queue, sets) : t) =
      let _rule, next_nt = x in
      let tbl_opt = sets.(next_nt) in
      let check, tbl =
        match tbl_opt with
        | Some tbl -> (Tbl.mem tbl x, tbl)
        | None ->
            let tbl = Tbl.create 16 in
            sets.(next_nt) <- Some tbl;
            (false, tbl)
      in
      if check then false
      else (
        Tbl.add tbl x ();
        Queue.push x queue;
        true )

    let pop_exn (queue, _) = Queue.take queue

    let[@landmark "array_all_with"] all_with_next_nt_incremented next_nt
        ((_queue, sets) : t) =
      let (*[@landmark "find tbl"]*) tbl_opt = sets.(next_nt) in
      match tbl_opt with
      | Some tbl ->
          let (*[@landmark "folddd"]*) r =
            Tbl.fold
              (fun ((rule, dot, origin), _next) () accum ->
                (rule, dot + 1, origin) :: accum)
              tbl []
          in
          r
      | None -> []
  end in
  let module Scan_queue = Unique_queue.Make (Scan_state) in
  (* For finding the next token for an item *)
  let (*[@landmark "recog_next"]*) next
      ({ Keyed_rule.rule = _lhs, rhs; _ }, dot, _origin) =
    if dot >= Array.length rhs then None else Some rhs.(dot)
  in

  (* Find nullable nonterminals (those that can produce the empty string)
     (see here http://loup-vaillant.fr/tutorials/earley-parsing/empty-rules) *)
  let nullable = Hashtbl.create 16 in
  let rec find_nullable () =
    let any_added =
      List.fold_left
        (fun any_added_so_far { Keyed_rule.rule = lhs, rhs; _ } ->
          let open Grammar in
          if any_added_so_far then true
          else if
            Array.for_all
              (function
                | Terminal _ -> false
                | Nonterminal nt -> Hashtbl.mem nullable nt)
              rhs
          then
            if not (Hashtbl.mem nullable lhs) then (
              Hashtbl.add nullable lhs ();
              true )
            else false
          else false)
        false rules
    in
    if any_added then find_nullable () else ()
  in
  find_nullable ();

  (* Initialise array of queues *)
  let state_sets = CCVector.create () in
  let create_state_set () =
    (Complete_queue.create (), Predict_queue.create (), Scan_queue.create ())
  in
  CCVector.push state_sets (create_state_set ());

  (* Add a state to the correct queue depending on its next token *)
  let add set state =
    let cq, pq, sq = set in
    let n = next state in
    match n with
    | None -> Complete_queue.push (state, ()) cq
    | Some (Grammar.Nonterminal nt) -> Predict_queue.push (state, nt) pq
    | Some (Grammar.Terminal t) -> Scan_queue.push (state, t) sq
  in

  let make_kth_set_if_needed k =
    if k >= CCVector.length state_sets then
      CCVector.push state_sets (create_state_set ())
  in

  (* Add a single state  *)
  let add_state k state =
    make_kth_set_if_needed k;
    let set = CCVector.get state_sets k in
    add set state
  in

  (* Add multiple states (but only check if a new set is needed once) *)
  let add_all k new_states =
    make_kth_set_if_needed k;
    let set = CCVector.get state_sets k in
    List.iter
      (fun new_state ->
        let _added = add set new_state in
        ())
      new_states
  in

  (* Completed items sorted by origin *)
  let completions = Array.make (Array.length tokens + 1) [] in

  (* Add the top level rule to the first set *)
  let top_level_rule = List.hd rules in
  assert (add (CCVector.get state_sets 0) (top_level_rule, 0, 0));

  (* Loop over the kth set until there are no changes *)
  let rec inner k =
    let finished =
      let set =
        try CCVector.get state_sets k
        with Invalid_argument _ ->
          (* Processing token k-1 didn't add any rules to set k *)
          let token_string =
            match token_to_string with Some f -> f tokens.(k - 1) | None -> ""
          in
          let context =
            match token_to_string with
            | Some f ->
                let window =
                  List.init 10 (fun i -> CCArray.get_safe tokens (k - i - 1))
                  |> List.filter_map (fun x -> x)
                  |> List.map f |> List.rev |> String.concat ""
                in
                Printf.sprintf " (context %s)" window
            | None -> ""
          in
          failwith
          @@ Printf.sprintf
               "Parsing failed: unexpected token %s at position %d%s"
               token_string (k - 1) context
      in
      let cq, pq, sq = set in
      match Predict_queue.is_empty pq with
      | false ->
          (* Predict *)
          let state, nonterminal = Predict_queue.pop_exn pq in
          let () =
            let () =
              if Hashtbl.mem nullable nonterminal then
                (* "magical completion" (see http://loup-vaillant.fr/tutorials/earley-parsing/empty-rules) *)
                let rule, dot, origin = state in
                let _added = add_state k (rule, dot + 1, origin) in
                ()
            in
            let new_states =
              rules_by_lhs.(nonterminal) |> List.map (fun rule -> (rule, 0, k))
            in
            add_all k new_states
          in
          Loop_unfinished
      | true -> (
          match Complete_queue.is_empty cq with
          | false ->
              (* Complete *)
              let state, () = Complete_queue.pop_exn cq in
              let ( ({ Keyed_rule.rule = lhs, _rhs; _ } as keyed_rule),
                    _dot,
                    origin ) =
                state
              in
              let new_states =
                let _, pq, _ = CCVector.get state_sets origin in
                Predict_queue.all_with_next_nt_incremented lhs pq
              in
              let completions_list = completions.(origin) in
              let () =
                completions.(origin) <-
                  ( ( decode_rule (array_rule_to_list_rule keyed_rule.rule),
                      keyed_rule.key ),
                    k )
                  :: completions_list
              in
              let () = add_all k new_states in
              Loop_unfinished
          | true -> (
              match Scan_queue.is_empty sq with
              | false ->
                  (* Scan *)
                  let state, terminal_predicate = Scan_queue.pop_exn sq in
                  let () =
                    match CCArray.get_safe tokens k with
                    | Some t when terminal_predicate t ->
                        let rule, dot, origin = state in
                        let _added =
                          add_state (k + 1) (rule, dot + 1, origin)
                        in
                        ()
                    | _ -> ()
                  in
                  Loop_unfinished
              | true ->
                  (* No items left in queues *)
                  Loop_finished ) )
    in

    (* Check if parse complete *)
    match finished with Loop_unfinished -> inner k | Loop_finished -> false
  in

  let rec outer k =
    (* Iterate inner loop on each token until either top level rule is complete
       or we run out of tokens. *)
    if k > Array.length tokens then ()
    else if inner k (* parse finished *) then ()
    else outer (k + 1)
  in

  let () = outer 0 in
  completions
