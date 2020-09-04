open Containers

let char_of_dec s = Char.chr (Int.of_string_exn s)

let char_of_bin s = Char.chr (Int.of_string_exn ("0b" ^ s))

let char_of_hex s = Char.chr (Int.of_string_exn ("0x" ^ s))

let char_in_range_incl ~lower ~upper x =
  let ( <= ) x y = Char.compare x y <= 0 in
  lower <= x && x <= upper
