(*
  I just read about the hamming code and wanted to write it myself to better understand it.
  This code is probably really bad, but it works...
*)
let rec pow x n = match n with
  | 0 -> 1
  | 1 -> n
  | n -> x * pow x (n - 1)

let getParityBitCount binary =
  let rec getCount n =
    let binaryFits = pow 2 n >= String.length binary + 1 in
    if binaryFits then n else getCount (n + 1) in
  getCount 0

let rec getParityBitPositions count = match count with
  | 1 -> [1]
  | x -> pow 2 x :: getParityBitPositions (x - 1)

let rec createBinaryList s = match s with
  | "" -> []
  | _ ->
    let hd = String.sub s 0 1 in
    let tl = String.sub s 1 (String.length s - 1) in
    int_of_string hd :: createBinaryList tl

let encodeBindary binary =
  let parityBitCount = getParityBitCount binary in
  let parityBitPositions = getParityBitPositions parityBitCount in
  let binaryList = createBinaryList binary in
  let rec createCode pos remainingCode =
    if remainingCode == [] then
      []
    else if List.exists (fun n -> n == pos) parityBitPositions then
      0 :: createCode (pos + 1) remainingCode
    else
      let hd = List.hd(remainingCode) in
      let tl = List.tl(remainingCode) in
      hd :: createCode (pos + 1) tl
    in
  createCode 1 binaryList

let rec print_binary = function
  [] -> print_string "\n"
  | hd::tl -> print_int hd ; print_string " " ; print_binary tl

let rec running () =
  let () = print_string "Type some binary: " in
  let binary = read_line () in
  let () = print_string "Code: " in
  let () = print_binary (encodeBindary binary) in
  running ();;

running ()
(* TODO add a fn to set the parity bits *)
(* TODO give a way to pick the node to change *)
(* TODO write the fixing fn for that error *)
