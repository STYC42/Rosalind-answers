type tree = Node of (tree list) | Leaf of string

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let (%+) a b =
  (((a+b) mod 1000000) + 1000000) mod 1000000

let (%*) a b =
  (((a*b) mod 1000000) + 1000000) mod 1000000

let rec sum_list l =
  match l with
  | [] -> 0
  | h :: t -> h %+ sum_list t

let build_tree s =
  let ic = Scanf.Scanning.from_string s in
  let rec aux acc =
    match Scanf.bscanf ic "%c" (fun a -> a) with
    |';' -> 
      assert (List.length acc = 1);
      Some (List.hd acc)
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

let nb_couples n =
  (n * (n-1)) / 2

let nb_quartets tr =
  let n_tot = tr |> leaves |> List.length in
  let rec aux par node =
    let n_int = node |> leaves |> List.length in
    let n_ext = (par |> leaves |> List.length) - n_int in
    match node with
    | Leaf _ -> 0
    | Node l ->
      let lower = l |> List.map (aux node) |> sum_list in
      let lower_couples = l |> List.map leaves 
                            |> List.map List.length 
                            |> List.map nb_couples in
      let sum_ncouples = sum_list lower_couples in
      let too_much = lower_couples |> List.map 
                                        (fun x -> x %* (sum_ncouples %+ (-x)))
                                   |> sum_list in
      let insid_gross = nb_couples n_ext %* nb_couples n_int in
      let insid = insid_gross %+ (-too_much/2) in
      let outsid = n_ext %* (n_tot - n_ext - n_int) %* nb_couples n_int in
      lower %+ insid %+ outsid
  in
  match tr with
  | Leaf _ -> 0
  | Node l ->
    let lower_couples = l |> List.map leaves 
                            |> List.map List.length 
                            |> List.map nb_couples in
    let sum_ncouples = sum_list lower_couples in
    let too_much = lower_couples |> List.map 
                                      (fun x -> x %* (sum_ncouples %+ (-x)))
                                  |> sum_list in
    List.fold_left (%+) 0 (List.map (aux tr) l) %+ (-too_much/2)
  
let main s =
  let lines = s |> String.split_on_char '\n' |> List.map String.trim in
  let tr = build_tree (List.nth lines 1) in
  Printf.printf "%d\n" (nb_quartets tr)

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
