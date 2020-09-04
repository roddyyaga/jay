type ('terminal, 'nonterminal) completion =
  (('terminal, 'nonterminal) Grammar.rule * int) * int

val recognise :
  ?token_to_string:('terminal -> string) ->
  ('terminal, 'nonterminal) Grammar.rule
  * ('terminal, 'nonterminal) Grammar.rule list ->
  'terminal list ->
  ('terminal, 'nonterminal) completion list array
