let consonants = "bcdfghjklmnpqrstvwxz"
let vowels = "aeiouy"

let digits =
  Array.init 120 (
    fun i ->
      let c = consonants.[i / 6] in
      let v = vowels.[i mod 6] in
      Printf.sprintf "%c%c" c v
  )

let digit1 =
  let tbl = Hashtbl.create 64 in
  for i = 0 to String.length consonants - 1 do
    Hashtbl.add tbl consonants.[i] (6 * i);
    Hashtbl.add tbl (Char.uppercase consonants.[i]) (6 * i);
  done;
  tbl

let digit2 =
  let tbl = Hashtbl.create 64 in
  for i = 0 to String.length vowels - 1 do
    Hashtbl.add tbl vowels.[i] i;
    Hashtbl.add tbl (Char.uppercase vowels.[i]) i;
  done;
  tbl

let decode c1 c2 =
  try Hashtbl.find digit1 c1 + Hashtbl.find digit2 c2
  with Not_found ->
    invalid_arg "Base120.decode"

let to_list n =
  let rec loop acc = function
      0 -> acc
    | n -> loop (digits.(n mod 120) :: acc) (n/120)
  in
  if n < 0 then
    "-" :: loop [] (-n)
  else if n = 0 then
    [digits.(0)]
  else
    loop [] n

let to_string n =
  String.concat "" (to_list n)

let of_string s =
  let rec loop s pos len n =
    if len < 0 then
      invalid_arg "Base120.of_string"
    else if len = 0 then n
    else
      let k = decode s.[pos] s.[pos+1] in
      loop s (pos + 2) (len - 2) (120 * n + k)
  in
  let len = String.length s in
  if len < 2 then
    invalid_arg "Base120.of_string";
  if s.[0] = '-' then
    - loop s 1 (len - 1) 0
  else
    loop s 0 len 0
