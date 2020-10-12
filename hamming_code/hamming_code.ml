(*
  I just read about the hamming code and wanted to write it myself to better understand it.
  This code is probably really bad, but it works...
*)
let rec pow x n = match n with
  | 0 -> 1
  | 1 -> x
  | n -> x * pow x (n - 1)

let rec print_binary = function
  [] -> print_string "\n"
  | hd::tl -> print_int hd ; print_string " " ; print_binary tl

let getParityBitCount codeSize =
  let rec getCount n =
    let binaryFits = pow 2 n >= codeSize + 1 in
    if binaryFits then n else getCount (n + 1) in
  getCount 0

let rec getParityBitPositions count = match count - 1 with
  | 0 -> [1]
  | x -> pow 2 x :: getParityBitPositions x

let rec createBinaryList s = match s with
  | "" -> []
  | _ ->
    let hd = String.sub s 0 1 in
    let tl = String.sub s 1 (String.length s - 1) in
    int_of_string hd :: createBinaryList tl

module IntMap = Map.Make(struct type t = int let compare = compare end)
let addParityCheck bit positions map =
  let rec fillCheck positions map = match positions with
    | [] -> map
    | hd::tl -> fillCheck tl (IntMap.add hd true map)
  in
  let checks = fillCheck positions IntMap.empty in
  IntMap.add bit checks map

let parityCheckMap = IntMap.empty
  |> addParityCheck 1 [1;3;5;7;9;11;13;15]
  |> addParityCheck 2 [2;3;6;7;10;11;14;15]
  |> addParityCheck 3 [4;5;6;7;12;13;14;15]
  |> addParityCheck 4 [8;9;10;11;12;13;14;15]

let setParityBits binary =
  let rec changeBit binary bit =
    let bitPos = pow 2 bit - 1 in
    if bitPos < List.length binary then
      let shouldCheckBit pos =
        try
          let checkMap = IntMap.find (bit + 1) parityCheckMap in
          IntMap.find pos checkMap
        with Not_found -> false in
      let clearOutBinary = List.mapi (fun i bit -> if shouldCheckBit i then bit else 0) binary in
      let bitCount = List.fold_left (fun acc n -> acc + n) 0 clearOutBinary in
      let newValue = if bitCount mod 2 == 0 then 0 else 1 in
      let newCode = List.mapi (fun i bit -> if i == bitPos then newValue else bit) binary in
      changeBit newCode (bit + 1)
    else
      binary
  in changeBit binary 0

let encodeBindary binary =
  let parityBitCount = getParityBitCount (List.length binary) in
  let parityBitPositions = getParityBitPositions parityBitCount in
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
  createCode 1 binary


let rec running () =
  let () = print_string "Type some binary: " in
  let binary = read_line () in
  let () = print_string "Code: " in
  let () = print_binary (setParityBits (encodeBindary (createBinaryList binary))) in
  running ();;

running ()
(* TODO give a way to pick the node to change *)
(* TODO write the fixing fn for that error *)
