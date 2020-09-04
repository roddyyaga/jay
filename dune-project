(lang dune 2.0)

(generate_opam_files true)

(name jay)

(version 0.1)

(source
 (github roddyyaga/jay))

(license MIT)

(authors "Roddy MacSween <github@roddymacsween.co.uk>")

(maintainers "Roddy MacSween <github@roddymacsween.co.uk>")

(package
 (name jay)
 (synopsis "Earley parser library for OCaml")
 (depends containers ppx_deriving))
