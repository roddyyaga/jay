type ('terminal, 'nonterminal) symbol =
  | Terminal of ('terminal -> bool)
  | Nonterminal of 'nonterminal
[@@deriving show]

type ('terminal, 'nonterminal) rule =
  'nonterminal * ('terminal, 'nonterminal) symbol list
[@@deriving show]

let string_of_rule string_of_nonterminal (lhs, rhs) =
  let left = string_of_nonterminal lhs in
  let right =
    rhs
    |> List.map (function
         | Terminal _tf -> "?"
         | Nonterminal nt -> string_of_nonterminal nt)
    |> String.concat " "
  in
  Printf.sprintf "%s -> %s" left right

type ('terminal, 'nonterminal) t =
  ('terminal, 'nonterminal) rule * ('terminal, 'nonterminal) rule list
