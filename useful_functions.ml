
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
