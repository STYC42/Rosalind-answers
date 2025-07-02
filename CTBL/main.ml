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
  if root != node then begin
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
    Array.iter print_int res;
    print_newline ()
  end

let main s =
  let root = build_tree s in
  let nds = nodes root in
  List.iter (split_tree root) nds


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
