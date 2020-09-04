let () = Printexc.record_backtrace true

let show_rule_string rule =
  Jay.Grammar.show_rule Format.pp_print_string Format.pp_print_string rule

let show_rule_char rule =
  Jay.Grammar.show_rule Format.pp_print_char Format.pp_print_string rule

let string_to_list s =
  s |> String.to_seq |> List.of_seq |> List.map (String.make 1)

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
    let open Jay.Ebnf.Infix in
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
                [ !+[ n "DIGIT" ] ]; [ !*[ n "DIGIT" ]; c '*'; !*[ n "DIGIT" ] ];
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
          [ c '('; !*[ n "c-wsp" ]; n "alternation"; !*[ n "c-wsp" ]; c ')' ] );
        ( "option",
          [ c '['; !*[ n "c-wsp" ]; n "alternation"; !*[ n "c-wsp" ]; c ']' ] );
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
                alt [ [ !+[ c '.'; !+[ n "BIT" ] ] ]; [ c '-'; !+[ n "BIT" ] ] ];
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
                    [ !+[ c '.'; !+[ n "DIGIT" ] ] ]; [ c '-'; !+[ n "DIGIT" ] ];
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
    let all_converted = Jay.Ebnf.compile_rules all_ebnf in
    let top_rule = List.hd all_converted in
    let other_rules = List.tl all_converted in
    (top_rule, other_rules)
  in
  let other_rules = Jay.Abnf.core_rules @ other_rules in
  (top_rule, other_rules)

let read_file_add_cr name =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop ((s ^ "\r\n") :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop [] |> String.concat "" |> String.to_seq |> List.of_seq

let custom_imap_rules =
  let ( ||| ) f g x = f x || g x in
  let ( &&& ) f g x = f x && g x in
  let ( !! ) f x = not @@ f x in
  let ctl = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false in
  let list_wildcards = function '%' | '*' -> true | _ -> false in
  let quoted_specials = function '"' | '\\' -> true | _ -> false in
  let resp_specials = function ']' -> true | _ -> false in
  let text_char = function '\r' | '\n' -> false | _ -> true in
  let atom_char =
    !!( (function '(' | ')' | '{' -> true | _ -> false)
      ||| ctl ||| list_wildcards ||| quoted_specials ||| resp_specials )
  in
  let astring_char = atom_char ||| resp_specials in

  let open Jay.Ebnf.Infix in
  [
    ("ATOM-CHAR", [ t atom_char ]);
    ("TEXT-CHAR", [ t text_char ]);
    ("QUOTED-CHAR", [ t (text_char &&& !!quoted_specials) ]);
    (* ^Another rule is left in imap.abnf *)
    ( "resp-text-code",
      [
        n "atom";
        !?[
            c ' ';
            !+[ t (text_char &&& !!(function ']' -> true | _ -> false)) ];
          ];
      ] );
    ( "tag",
      [ !+[ t (astring_char &&& !!(function '+' -> true | _ -> false)) ] ] );
  ]

let abnf_spec = read_file_add_cr "abnf.abnf"

let completions =
  Jay.Parser.recognise ~token_to_string:(String.make 1) abnf abnf_spec

let tree = Jay.Trees.build_one completions abnf_spec

let split_rules all_rules top_level_key =
  let top_rule =
    List.find (fun (rule, _) -> String.equal rule top_level_key) all_rules
  in
  let other_rules =
    List.filter
      (fun (rule, _) -> not @@ String.equal rule top_level_key)
      all_rules
  in
  (top_rule, other_rules)

let () =
  match tree with
  | None -> print_endline "Couldn't build tree for ABNF spec"
  | Some tree -> (
      Jay.Trees.print_tree (fun x -> x) (String.make 1) tree;
      let ebnf = Jay.Abnf.tree_to_ebnf_tree (Nt tree) in
      print_endline "With EBNF features reconstructed:";
      print_endline @@ Jay.Abnf.show_ebnf_tree Format.pp_print_char ebnf;

      let rules = Jay.Abnf.ebnf_tree_to_rulelist ebnf in
      print_endline "Rules:";
      print_endline @@ Jay.Abnf.show_rulelist rules;

      let ebnf = Jay.Abnf.rulelist_to_ebnf rules in
      print_endline "Ebnf:";
      print_endline @@ Jay.Abnf.show_ebnf_rules ebnf;

      let all_rules = Jay.Ebnf.compile_rules ebnf in
      let top_rule, other_rules = split_rules all_rules "rulelist" in
      let spec = read_file_add_cr "abnf.abnf" in
      let new_completions =
        Jay.Parser.recognise ~token_to_string:(String.make 1)
          (top_rule, other_rules) spec
      in
      let tree = Jay.Trees.build_one new_completions spec in
      match tree with
      | None ->
          print_endline
            "Couldn't build tree for ABNF spec parsed with ABNF spec"
      | Some tree -> (
          let ebnf = Jay.Abnf.tree_to_ebnf_tree (Nt tree) in
          print_endline "With EBNF features reconstructed:";
          print_endline @@ Jay.Abnf.show_ebnf_tree Format.pp_print_char ebnf;

          let rules = Jay.Abnf.ebnf_tree_to_rulelist ebnf in
          print_endline "Rules:";
          print_endline @@ Jay.Abnf.show_rulelist rules;

          let ebnf = Jay.Abnf.rulelist_to_ebnf rules in
          print_endline "Ebnf:";
          print_endline @@ Jay.Abnf.show_ebnf_rules ebnf;

          let all_rules = Jay.Ebnf.compile_rules ebnf in
          let top_rule, other_rules = split_rules all_rules "rulelist" in
          let spec = read_file_add_cr "imap.abnf" in
          let new_completions =
            Jay.Parser.recognise ~token_to_string:(String.make 1)
              (top_rule, other_rules) spec
          in
          let tree = Jay.Trees.build_one new_completions spec in
          match tree with
          | None -> print_endline "Couldn't build tree for IMAP spec"
          | Some tree -> (
              let ebnf = Jay.Abnf.tree_to_ebnf_tree (Nt tree) in
              print_endline "With EBNF features reconstructed:";
              print_endline @@ Jay.Abnf.show_ebnf_tree Format.pp_print_char ebnf;

              let rules = Jay.Abnf.ebnf_tree_to_rulelist ebnf in
              print_endline "Rules:";
              print_endline @@ Jay.Abnf.show_rulelist rules;

              let imap_rules =
                Jay.Abnf.core_rules
                @ Jay.Ebnf.compile_rules
                    (custom_imap_rules @ Jay.Abnf.rulelist_to_ebnf rules)
              in
              let top_rule, other_rules = split_rules imap_rules "response" in
              let imap_response = read_file_add_cr "imap_response.txt" in
              let new_completions =
                Jay.Parser.recognise ~token_to_string:(String.make 1)
                  (top_rule, other_rules) imap_response
              in
              let tree = Jay.Trees.build_one new_completions imap_response in
              match tree with
              | None -> print_endline "Couldn't build tree for IMAP message"
              | Some tree ->
                  print_endline "Parsed IMAP:";
                  let ebnf = Jay.Abnf.tree_to_ebnf_tree (Nt tree) in
                  print_endline
                  @@ Jay.Abnf.show_ebnf_tree Format.pp_print_char ebnf ) ) )
