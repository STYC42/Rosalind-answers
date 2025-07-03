module SplitSet = Set.Make(String)

type tree = Node of (tree list) | Leaf of string

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let build_tree s =
  let ic = Scanf.Scanning.from_string s in
  let rec aux acc =
    match Scanf.bscanf ic "%c" (fun a -> a) with
    |';' -> Some (List.hd acc)
    | ')' -> begin
      match acc with
      | [] -> None
      | _h :: _t -> Some (Node acc) end
    | '(' -> begin
      match aux [] with
      | None -> aux acc
      | Some child -> aux (child :: acc) end
    | ',' -> aux acc
    | e -> 
      let o = Scanf.bscanf ic "%[^,)]" (fun a -> a) in
      aux (Leaf ((String.make 1 e)^o) :: acc)
  in
  match aux [] with
  | None -> failwith "Tree is empty"
  | Some a -> a

let rec leaves node =
  match node with
  | Leaf a -> [a]
  | Node l -> l |> List.map leaves
                |> List.flatten

let rec nodes node =
  match node with
  | Leaf _ -> []
  | Node l -> node :: (l |> List.map nodes
                         |> List.flatten )
  
let split_tree root node =
  assert (root != node);
  let pos = Hashtbl.create 1 in
  root |> leaves
      |> List.sort compare
      |> List.iteri (fun i lv -> Hashtbl.add pos lv i);
  let sep = leaves node in
  let res = Array.make (List.length (leaves root)) 0 in
  List.iter (
    fun c ->
      res.(Hashtbl.find pos c) <- 1
  ) sep;
  if res.(0) = 0 then Array.iteri (fun i x -> res.(i) <- 1-x) res;
  res |> Array.to_list
      |> List.map string_of_int
      |> String.concat ""

let main s =
  let lines = String.split_on_char '\n' s in
  let l = lines |> List.hd |> String.split_on_char ' ' in
  let root1 = build_tree (List.nth lines 1) in
  let root2 = build_tree (List.nth lines 2) in
  let nds1 = nodes root1 in
  let nds2 = nodes root2 in
  let set1 = nds1 |> List.tl
                  |> List.map (split_tree root1)
                  |> SplitSet.of_list in
  let set2 = nds2 |> List.tl
                  |> List.map (split_tree root2)
                  |> SplitSet.of_list in
  let inter = SplitSet.inter set1 set2 in
  Printf.printf "%d\n" (2*(List.length l - 3) - 2*(SplitSet.cardinal inter))


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
