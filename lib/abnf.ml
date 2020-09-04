open Containers

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

let lhs completion =
  let ((lhs, _), _), _, _ = completion in
  lhs

let rec node_string = function
  | Rule (_completion, children) ->
      children |> List.map node_string |> String.concat ""
  | Term t -> String.make 1 t
  | Star children | Plus children ->
      children |> List.concat |> List.map node_string |> String.concat ""
  | Option (Some children) ->
      children |> List.map node_string |> String.concat ""
  | Option None -> ""

let remove_alts root =
  let open Trees in
  let rec aux = function
    | Nt (completion, children) -> (
        match (lhs completion, children) with
        | rule, children when String.suffix ~suf:"_alt" rule ->
            List.concat_map aux children
        | _other_rule, children ->
            [ Nt (completion, List.concat_map aux children) ] )
    | T c -> [ T c ]
  in
  match root with
  | Nt (completion, children) -> Nt (completion, List.concat_map aux children)
  | T c -> T c

let tree_to_ebnf_tree node =
  let open Trees in
  let rec aux = function
    | Nt (completion, children) -> (
        match (lhs completion, children) with
        | rule, [] when String.suffix ~suf:"_star" rule -> Star []
        | rule, many when String.suffix ~suf:"_star" rule -> (
            let next, direct = List.hd_tl (List.rev many) in
            let remainder = aux next in
            match remainder with
            | Star remainder ->
                Star (List.map aux (List.rev direct) :: remainder)
            | _ -> assert false )
        | rule, [ child ] when String.suffix ~suf:"_plus" rule ->
            Plus [ [ aux child ] ]
        | rule, many when String.suffix ~suf:"_plus" rule -> (
            let next, direct = List.hd_tl (List.rev many) in
            let remainder = aux next in
            match remainder with
            | Plus remainder ->
                Plus (List.map aux (List.rev direct) :: remainder)
            | _ -> assert false )
        | rule, [] when String.suffix ~suf:"_option" rule -> Option None
        | rule, children when String.suffix ~suf:"_option" rule ->
            Option (Some (List.map aux children))
        | other_rule, children -> Rule (other_rule, List.map aux children) )
    | T c -> Term c
  in
  aux (remove_alts node)

let show_ebnf_list xs =
  xs |> List.map (show_ebnf_tree Format.pp_print_char) |> String.concat "; "

let general_num_val char_of_digits first_digs list_or_range =
  let terminal_of_char s =
    let c = char_of_digits s in
    fun x -> Char.equal x c
  in
  match list_or_range with
  | None ->
      (* Just one digit *)
      Terminals [ terminal_of_char (node_string first_digs) ]
  | Some more_digits -> (
      match more_digits with
      | [ Term '-'; second_digs ] ->
          let lower = char_of_digits (node_string first_digs) in
          let upper = char_of_digits (node_string second_digs) in
          Terminals [ Utils.char_in_range_incl ~lower ~upper ]
      | [ Plus char_list ] ->
          let following_terminals =
            ListLabels.map char_list ~f:(function
              | [ Term '.'; element_digs ] ->
                  terminal_of_char (node_string element_digs)
              | _ -> assert false)
          in
          Terminals
            (terminal_of_char (node_string first_digs) :: following_terminals)
      | _ -> assert false )

let rec ebnf_tree_to_rulelist = function
  | Rule ("rulelist", [ Plus children ]) ->
      children |> List.concat |> List.filter_map make_rule
  | _ -> assert false

and make_rule = function
  | Rule ("rule", [ rulename; _defined_as; elements; _c_nl ]) ->
      Some (node_string rulename, make_elements elements)
  | _ -> None

and make_elements = function
  | Rule ("elements", [ alternation; _c_wsp ]) -> make_alternation alternation
  | _ -> assert false

and make_alternation = function
  | Rule ("alternation", [ first_concat; Star other_concats ]) ->
      let first = make_concatenation first_concat in
      let rest =
        List.map
          (function
            | [ Star _c_wsp; Term '/'; Star _c_wsp'; concatenation ] ->
                make_concatenation concatenation
            | _ -> assert false)
          other_concats
      in
      first :: rest
  | _ -> assert false

and make_concatenation = function
  | Rule ("concatenation", [ first_rep; Star other_reps ]) ->
      let first = make_repetition first_rep in
      let rest =
        List.map
          (function
            | [ Plus _c_wsp; repetition ] -> make_repetition repetition
            | _ -> assert false)
          other_reps
      in
      first :: rest
  | _ -> assert false

and make_repetition = function
  | Rule ("repetition", [ Option repeat; element ]) ->
      let repeat =
        match repeat with
        | Some [ r ] -> make_repeat r
        | None -> Range (None, None)
        | Some other ->
            print_endline @@ show_ebnf_list other;
            assert false
      in
      (repeat, make_element element)
  | _ -> assert false

and make_element = function
  | Rule ("element", [ content ]) -> make_element_content content
  | _ -> assert false

and make_element_content node =
  match node with
  | Rule ("rulename", _children) -> Nonterminal (node_string node)
  | Rule ("group", children) -> (
      match children with
      | [ Term '('; Star _c_wsp; alternation; Star _c_wsp'; Term ')' ] ->
          Group (make_alternation alternation)
      | _ -> assert false )
  | Rule ("option", children) -> (
      match children with
      | [ Term '['; Star _c_wsp; alternation; Star _c_wsp'; Term ']' ] ->
          Option (make_alternation alternation)
      | _ -> assert false )
  | Rule ("char-val", children) -> make_char_val children
  | Rule ("num-val", children) -> make_num_val children
  | Rule ("prose-val", _children) ->
      failwith
      @@ Printf.sprintf
           "Cannot process rule with terminal described in prose (%s)."
           (node_string node)
  | _ -> assert false

and make_char_val = function
  | [ Rule ("DQUOTE", _); Star content; Rule ("DQUOTE", _) ] ->
      Terminals
        (ListLabels.map content ~f:(function
          | [ Term c ] ->
              fun x -> Char.(equal (lowercase_ascii x) (lowercase_ascii c))
          | _ -> assert false))
  | _ -> assert false

and make_num_val = function
  | [ Term '%'; Rule ("bin-val", bin_val) ] -> make_bin_val bin_val
  | [ Term '%'; Rule ("dec-val", dec_val) ] -> make_dec_val dec_val
  | [ Term '%'; Rule ("hex-val", hex_val) ] -> make_hex_val hex_val
  | other ->
      print_endline @@ show_ebnf_list other;
      assert false

and make_dec_val = function
  | [ Term 'd'; first_digs; Option list_or_range ] ->
      general_num_val Utils.char_of_dec first_digs list_or_range
  | _ -> assert false

and make_bin_val = function
  | [ Term 'b'; first_digs; Option list_or_range ] ->
      general_num_val Utils.char_of_bin first_digs list_or_range
  | _ -> assert false

and make_hex_val = function
  | [ Term 'x'; first_digs; Option list_or_range ] ->
      general_num_val Utils.char_of_hex first_digs list_or_range
  | other ->
      print_endline @@ show_ebnf_list other;
      assert false

and make_repeat = function
  | Rule ("repeat", [ digits ]) -> Fixed (int_of_string (node_string digits))
  | Rule ("repeat", [ minimum; Term '*'; maximum ]) ->
      let minimum =
        match node_string minimum with
        | "" -> Some 0
        | s -> Some (int_of_string s)
      in
      let maximum =
        match node_string maximum with
        | "" -> None
        | s -> Some (int_of_string s)
      in
      Range (minimum, maximum)
  | _ -> assert false

let rec ebnf_of_alternation alternation =
  let alt_content =
    List.map
      (fun concatenation ->
        List.concat_map
          (fun (repeat, element) ->
            let ebnf_list = ebnf_list_of_element element in
            match repeat with
            | Range (None, None) -> ebnf_list
            | Range (Some 0, None) -> [ Ebnf.Star ebnf_list ]
            | Range (Some 1, None) -> [ Ebnf.Plus ebnf_list ]
            | Range (Some lower, Some upper) ->
                let alternation =
                  List.init
                    (upper - lower + 1)
                    (fun i ->
                      let count = lower + i - 1 in
                      List.init count (fun _ -> ebnf_list) |> List.concat)
                in
                [ Ebnf.Alt alternation ]
            | Fixed digits ->
                List.init digits (fun _ -> ebnf_list) |> List.concat
            | _ -> failwith "Unsupported repeat number")
          concatenation)
      alternation
  in
  Ebnf.Alt alt_content

and ebnf_list_of_element = function
  | Terminals ts -> List.map (fun t -> Ebnf.Term t) ts
  | Nonterminal nt -> [ Ebnf.Nonterm nt ]
  | Group alternation -> [ ebnf_of_alternation alternation ]
  | Option alternation -> [ Option [ ebnf_of_alternation alternation ] ]

let rulelist_to_ebnf =
  List.map (fun (nonterminal, alternation) ->
      (nonterminal, [ ebnf_of_alternation alternation ]))

let core_rules =
  (*
         ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z

         BIT            =  "0" / "1"

         CHAR           =  %x01-7F
                                ; any 7-bit US-ASCII character,
                                ;  excluding NUL



Crocker & Overell           Standards Track                    [Page 13]

 
RFC 5234                          ABNF                      January 2008


         CR             =  %x0D
                                ; carriage return

         CRLF           =  CR LF
                                ; Internet standard newline

         CTL            =  %x00-1F / %x7F
                                ; controls

         DIGIT          =  %x30-39
                                ; 0-9

         DQUOTE         =  %x22
                                ; " (Double Quote) "

         HEXDIG         =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"

         HTAB           =  %x09
                                ; horizontal tab

         LF             =  %x0A
                                ; linefeed

         LWSP           =  *(WSP / CRLF WSP)
                                ; Use of this linear-white-space rule
                                ;  permits lines containing only white
                                ;  space that are no longer legal in
                                ;  mail headers and have caused
                                ;  interoperability problems in other
                                ;  contexts.
                                ; Do not use when defining mail
                                ;  headers and use with caution in
                                ;  other contexts.

         OCTET          =  %x00-FF
                                ; 8 bits of data

         SP             =  %x20

         VCHAR          =  %x21-7E
                                ; visible (printing) characters

         WSP            =  SP / HTAB
                                ; white space *)
  let open Grammar in
  let n x = Nonterminal x in
  let c x =
    Terminal (fun y -> Char.(equal (lowercase_ascii x) (lowercase_ascii y)))
  in
  let alt xs =
    Terminal
      (fun y ->
        List.exists
          (fun x -> Char.(equal (lowercase_ascii x) (lowercase_ascii y)))
          xs)
  in
  [
    ( "ALPHA",
      [
        Terminal
          (function
          | '\x41' .. '\x5A' -> true | '\x61' .. '\x7A' -> true | _ -> false);
      ] );
    (* A-Z / a-z *)
    ("BIT", [ Terminal (function '0' | '1' -> true | _ -> false) ]);
    ("CHAR", [ Terminal (function '\x01' .. '\x7F' -> true | _ -> false) ]);
    (* any 7-bit US-ASCII character,
        excluding NUL *)
    ("CR", [ c '\x0D' ]);
    (* Carriage return *)
    ("CRLF", [ n "CR"; n "LF" ]);
    (* Internet standard newline *)
    ( "CTL",
      [ Terminal (function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false) ] );
    (* Controls *)
    ("DIGIT", [ Terminal (function '\x30' .. '\x39' -> true | _ -> false) ]);
    (* 0-9 *)
    ("DQUOTE", [ c '\x22' ]);
    (* (Double Quote ) *)
    ("HEXDIG", [ n "DIGIT" ]);
    ("HEXDIG", [ alt [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F' ] ]);
    ("HTAB", [ c '\x09' ]);
    (* horizontal tab *)
    ("LF", [ c '\x0A' ]);
    (* linefeed *)
    ("LWSP", [ n "WSP"; n "LWSP" ]);
    ("LWSP", [ n "CRLF"; n "WSP"; n "LWSP" ]);
    ("LWSP", []);
    (* Use of this linear-white-space rule
        permits lines containing only white
        space that are no longer legal in
        mail headers and have caused
        interoperability problems in other
        contexts.
       Do not use when defining mail
        headers and use with caution in
        other contexts.*)
    ("OCTET", [ Terminal (function '\x00' .. '\xFF' -> true) ]);
    (* 8 bits of data *)
    ("SP", [ c '\x20' ]);
    ("VCHAR", [ Terminal (function '\x21' .. '\x7E' -> true | _ -> false) ]);
    (* visible (printing) characters *)
    ("WSP", [ n "SP" ]);
    ("WSP", [ n "HTAB" ]);
    (* white space *)
  ]

let parse s =
  let abnf =
    (*

         rulelist       =  1*( rule / ( * c-wsp c-nl) )

         rule           =  rulename defined-as elements c-nl
                                ; continues if next line starts
                                ;  with white space

         rulename       =  ALPHA *(ALPHA / DIGIT / "-")


         defined-as     =  *c-wsp ("=" / "=/") *c-wsp
                                ; basic rules definition and
                                ;  incremental alternatives

         elements       =  alternation *c-wsp

         c-wsp          =  WSP / (c-nl WSP)

         c-nl           =  comment / CRLF
                                ; comment or newline

         comment        =  ";" *(WSP / VCHAR) CRLF

         alternation    =  concatenation
                           *( *c-wsp "/" *c-wsp concatenation)

         concatenation  =  repetition *(1*c-wsp repetition)

         repetition     =  [repeat] element

         repeat         =  1*DIGIT / ( *DIGIT "*" *DIGIT)

         element        =  rulename / group / option /
                           char-val / num-val / prose-val

         group          =  "(" *c-wsp alternation *c-wsp ")"

         option         =  "[" *c-wsp alternation *c-wsp "]"

         char-val       =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE
                                ; quoted string of SP and VCHAR
                                ;  without DQUOTE

         num-val        =  "%" (bin-val / dec-val / hex-val)

         bin-val        =  "b" 1*BIT
                           [ 1*("." 1*BIT) / ("-" 1*BIT) ]
                                ; series of concatenated bit values
                                ;  or single ONEOF range

         dec-val        =  "d" 1*DIGIT
                           [ 1*("." 1*DIGIT) / ("-" 1*DIGIT) ]

         hex-val        =  "x" 1*HEXDIG
                           [ 1*("." 1*HEXDIG) / ("-" 1*HEXDIG) ] 

         prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"
                                ; bracketed string of SP and VCHAR
                                ;  without angles
                                ; prose description, to be used as
                                ;  last resort *)
    let top_rule, other_rules =
      let open Ebnf.Infix in
      let all_ebnf =
        [
          ( "rulelist",
            [ !+[ alt [ [ n "rule" ]; [ !*[ n "c-wsp" ]; n "c-nl" ] ] ] ] );
          ("rule", [ n "rulename"; n "defined-as"; n "elements"; n "c-nl" ]);
          (* continues if next line starts
              with white space *)
          ( "rulename",
            [ n "ALPHA"; !*[ alt [ [ n "ALPHA" ]; [ n "DIGIT" ]; [ c '-' ] ] ] ]
          );
          ( "defined-as",
            [
              !*[ n "c-wsp" ];
              alt [ [ c '=' ]; [ c '='; c '/' ] ];
              !*[ n "c-wsp" ];
            ] );
          (* basic rules definition and
              incremental alternatives *)
          ("elements", [ n "alternation"; !*[ n "c-wsp" ] ]);
          ("c-wsp", [ alt [ [ n "WSP" ]; [ n "c-nl"; n "WSP" ] ] ]);
          ("c-nl", [ alt [ [ n "comment" ]; [ n "CRLF" ] ] ]);
          (* comment or newline *)
          ( "comment",
            [ c ';'; !*[ alt [ [ n "WSP" ]; [ n "VCHAR" ] ] ]; n "CRLF" ] );
          ( "alternation",
            [
              n "concatenation";
              !*[ !*[ n "c-wsp" ]; c '/'; !*[ n "c-wsp" ]; n "concatenation" ];
            ] );
          ( "concatenation",
            [ n "repetition"; !*[ !+[ n "c-wsp" ]; n "repetition" ] ] );
          ("repetition", [ !?[ n "repeat" ]; n "element" ]);
          ( "repeat",
            [
              alt
                [
                  [ !+[ n "DIGIT" ] ];
                  [ !*[ n "DIGIT" ]; c '*'; !*[ n "DIGIT" ] ];
                ];
            ] );
          ( "element",
            [
              alt
                [
                  [ n "rulename" ];
                  [ n "group" ];
                  [ n "option" ];
                  [ n "char-val" ];
                  [ n "num-val" ];
                  [ n "prose-val" ];
                ];
            ] );
          ( "group",
            [ c '('; !*[ n "c-wsp" ]; n "alternation"; !*[ n "c-wsp" ]; c ')' ]
          );
          ( "option",
            [ c '['; !*[ n "c-wsp" ]; n "alternation"; !*[ n "c-wsp" ]; c ']' ]
          );
          ( "char-val",
            [
              n "DQUOTE";
              !*[
                  t (function
                    | '\x20' .. '\x21' -> true
                    | '\x23' .. '\x7E' -> true
                    | _ -> false);
                ];
              n "DQUOTE";
            ] );
          (* quoted string of SP and VCHAR
              without DQUOTE *)
          ( "num-val",
            [ c '%'; alt [ [ n "bin-val" ]; [ n "dec-val" ]; [ n "hex-val" ] ] ]
          );
          ( "bin-val",
            [
              c 'b';
              !+[ n "BIT" ];
              !?[
                  alt
                    [ [ !+[ c '.'; !+[ n "BIT" ] ] ]; [ c '-'; !+[ n "BIT" ] ] ];
                ];
            ] );
          (* series of concatenated bit values
              or single ONEOF range *)
          ( "dec-val",
            [
              c 'd';
              !+[ n "DIGIT" ];
              !?[
                  alt
                    [
                      [ !+[ c '.'; !+[ n "DIGIT" ] ] ];
                      [ c '-'; !+[ n "DIGIT" ] ];
                    ];
                ];
            ] );
          ( "hex-val",
            [
              c 'x';
              !+[ n "HEXDIG" ];
              !?[
                  alt
                    [
                      [ !+[ c '.'; !+[ n "HEXDIG" ] ] ];
                      [ c '-'; !+[ n "HEXDIG" ] ];
                    ];
                ];
            ] );
          ( "prose-val",
            [
              c '<';
              !*[
                  t (function
                    | '\x20' .. '\x3D' -> true
                    | '\x3F' .. '\x7E' -> true
                    | _ -> false);
                ];
              c '>';
            ] );
          (* bracketed string of SP and VCHAR
              without angles
             prose description, to be used as
              last resort *)
        ]
      in
      let all_converted = Ebnf.compile_rules all_ebnf in
      let top_rule = List.hd all_converted in
      let other_rules = List.tl all_converted in
      (top_rule, other_rules)
    in
    let other_rules = core_rules @ other_rules in
    (top_rule, other_rules)
  in
  let s = String.to_list s in
  let completions = Parser.recognise ~token_to_string:(String.make 1) abnf s in
  match Trees.build_one completions s with
  | Some tree ->
      tree_to_ebnf_tree (Nt tree)
      |> ebnf_tree_to_rulelist |> rulelist_to_ebnf |> Ebnf.compile_rules
  | None -> failwith "Couldn't build parse tree"
