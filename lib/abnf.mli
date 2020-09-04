(** Defines functionality for parsing ABNF specifications and using them to parse other strings *)

type 't ebnf_tree =
  | Rule of string * 't ebnf_tree list
  | Term of 't
  | Star of 't ebnf_tree list list
  | Plus of 't ebnf_tree list list
  | Option of 't ebnf_tree list option
[@@deriving show]

type repeat = Fixed of int | Range of int option * int option
[@@deriving show]

type alternation = concatenation list [@@deriving show]

and concatenation = repetition list [@@deriving show]

and repetition = repeat * element [@@deriving show]

and element =
  | Terminals of (char -> bool) list
  | Nonterminal of string
  | Group of alternation
  | Option of alternation
[@@deriving show]

type rulelist = (string * alternation) list [@@deriving show]

type ebnf_rules = (string * (char -> bool) Ebnf.t list) list [@@deriving show]

val core_rules : (char, string) Grammar.rule list

val parse : string -> (char, string) Grammar.rule list

val tree_to_ebnf_tree : ('a, string) Trees.child -> 'a ebnf_tree

val ebnf_tree_to_rulelist : char ebnf_tree -> rulelist

val rulelist_to_ebnf : rulelist -> ebnf_rules
