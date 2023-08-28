(* sort tuples ('a * int) *)
let sort_array lst = List.sort (fun (a1, a2) (b1, b2) -> compare a2 b2) lst

(* flatten 'a list list -> 'a list *)
let flatten lst =
  let sub acc lst =
    let rec rsub acc = function
    | [] -> acc
    | h :: t -> h :: rsub acc t in
    List.rev (rsub acc lst) in
  let rec aux acc = function
  | [] -> acc
  | h :: t -> aux (sub acc h) t in
  aux [] lst

(* explode string -> char list *)
let explode (s: string) = List.init (String.length s) (String.get s)

(* is_prime fn *)
let is_prime n =
  let rec aux m n = m * m > n || (n mod m != 0 && aux (m+1) n)
  in aux 2 (abs n)

(* range fn *)
let rec range m n = if m < n then m :: aux (m + 1) n else [n]

let rec range_steps ?(step=1) n m =
  if n > m then []
  else n :: range_steps ~step (n+step) m

let range_asc_desc n m =
  let rec asc n m = if n < m then n :: asc (n+1) m else [m] in
  let rec desc n m = if n > m then n :: desc (n-1) m else [m] in
  if n < m then asc n m else desc n m

(* making operators *)
let rec sum = function
  | [] -> 0
  | h :: t -> h + sum t

(* making operators *)
let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t 

(* making a fold *)
let rec fold ~init:init ~f:op = function
  | [] -> init
  | h :: t -> op h (fold ~init:init ~f:op t)

let prm = [2; 3; 5; 7; 11; 13]
let hl = ["hello"; " "; "world"; "!"]

let sum'    lst = fold ~init:0  ~f:( + ) lst
let concat' lst = fold ~init:"" ~f:( ^ ) lst

(* making fold right *)
let rec fold_right ~init:acc ~f:f lst =
  match lst with
  | [] -> acc
  | h :: t -> f h (fold_right ~init:acc ~f:f t)

(* testing fold_left and fold_right *)
let sum1fl lst = List.fold_left ~f:( + ) ~init:0 lst
let sum2fl lst = List.fold_left ~f:(fun a b -> a + b) ~init:0 lst
let prodfl lst = List.fold_left ~f:(fun a b -> a * b) ~init:0 lst
let sum1fr lst = List.fold_right lst ~f:( + ) ~init:0
let sum2fr lst = List.fold_right lst ~f:(fun a b -> a + b) ~init:0
let prodfr lst = List.fold_right lst ~f:(fun a b -> a * b) ~init:0

let conc1fl lst = List.fold_left ~f:( ^ ) ~init:"" lst
let conc2fl lst = List.fold_left ~f:(fun a b -> a ^ b) ~init:"" lst
let conc3fl lst = List.fold_left ~f:(fun a b -> ( ^ ) a b) ~init:"" lst
let conc1fr lst = List.fold_right lst ~f:( ^ ) ~init:""
let conc2fr lst = List.fold_right lst ~f:(fun a b -> a ^ b) ~init:""
let conc3fr lst = List.fold_right lst ~f:(fun a b -> ( ^ ) a b) ~init:""
