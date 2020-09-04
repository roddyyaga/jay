(** Defines a way to specify grammars using an extended version of BNF,
   with alternations, optional sections, and regex-style operators * and +
   for zero/one-or-more repetitions *)

type 't t =
  | Term of 't
  | Nonterm of string
  | Alt of 't t list list
  | Option of 't t list
  | Star of 't t list
  | Plus of 't t list
[@@deriving show]

let rec compile_seq fresh seq =
  List.fold_left
    (fun (rules_accum, processed_symbols) symbol ->
      let open Grammar in
      match symbol with
      | Term t -> (rules_accum, Terminal t :: processed_symbols)
      | Nonterm s -> (rules_accum, Nonterminal s :: processed_symbols)
      | Alt seqs ->
          let group_symbol = fresh () ^ "_alt" in
          let new_rules =
            seqs
            |> List.map (compile_seq fresh)
            |> List.concat_map (fun (rules', rhs) ->
                   (group_symbol, rhs) :: rules')
          in
          ( rules_accum @ new_rules,
            Nonterminal group_symbol :: processed_symbols )
      | Option seq ->
          let group_symbol = fresh () ^ "_option" in
          let recursed_rules, group_content = compile_seq fresh seq in
          let new_rules =
            [ (group_symbol, group_content); (group_symbol, []) ]
          in
          ( rules_accum @ new_rules @ recursed_rules,
            Nonterminal group_symbol :: processed_symbols )
      | Star seq ->
          let group_symbol = fresh () ^ "_star" in
          let recursed_rules, group_content = compile_seq fresh seq in
          let new_rules =
            [
              (group_symbol, group_content @ [ Nonterminal group_symbol ]);
              (group_symbol, []);
            ]
          in
          ( rules_accum @ new_rules @ recursed_rules,
            Nonterminal group_symbol :: processed_symbols )
      | Plus seq ->
          let group_symbol = fresh () ^ "_plus" in
          let recursed_rules, group_content = compile_seq fresh seq in
          let new_rules =
            [
              (group_symbol, group_content @ [ Nonterminal group_symbol ]);
              (group_symbol, group_content);
            ]
          in
          ( rules_accum @ new_rules @ recursed_rules,
            Nonterminal group_symbol :: processed_symbols ))
    ([], []) seq
  |> fun (rules, symbols) -> (rules, List.rev symbols)

let compile_rules rules =
  let used_nonterminals = ref (List.map fst rules) in
  let fresh () =
    let rec iter n =
      let symbol = Int.to_string n in
      if not (List.mem symbol !used_nonterminals) then (
        used_nonterminals := symbol :: !used_nonterminals;
        symbol )
      else iter (n + 1)
    in
    iter 1
  in
  List.concat_map
    (fun (lhs, rhs) ->
      let generated_rules, rhs' = compile_seq fresh rhs in
      (lhs, rhs') :: generated_rules)
    rules

module Infix = struct
  let t x = Term x

  let c x = Term (fun y -> x = y)

  let n x = Nonterm x

  let ( !* ) x = Star x

  let ( !+ ) x = Plus x

  let ( !? ) x = Option x

  let alt x = Alt x
end
