type tree = Nil | Node of tree

module TreeTbl = Hashtbl.Make(struct
  type t = tree
  let equal = (==)  (* égalité physique *)
  let hash = Hashtbl.hash
end)

let split_on_double_newline s =
  Str.split (Str.regexp "\n\n") s

let parse_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let blocks = split_on_double_newline content in
  List.map (fun block ->
    match String.split_on_char '\n' (String.trim block) with
    | [desc_line; words_line] ->
        let description = desc_line in
        let word1, word2 =
          match String.split_on_char ' ' words_line with
          | [w1; w2] -> (w1, w2)
          | _ -> failwith ("Ligne de mots invalide : " ^ words_line)
        in
        (description, word1, word2)
    | _ -> failwith ("Bloc invalide : " ^ block)
  ) blocks


let build tree =
  let ic = Scanf.Scanning.from_string tree in
  let names = Hashtbl.create 1 in
  let weights = TreeTbl.create 1 in
  let rec explore p =
    let self = Node (p) in
    let rec aux () =
      Scanf.bscanf ic "%c" (
        fun c ->
          match c with
          | ')' -> aux ()
          | ';' -> TreeTbl.replace weights self 0;
                   TreeTbl.replace weights p 0
          | '(' -> explore self; 
                   aux ()
          | ',' -> explore self;
                   aux ()
          | ':' -> Scanf.bscanf ic "%d" (
                     TreeTbl.replace weights self
                   )
          | c -> Scanf.bscanf ic "%[^:]:%d" (
                   fun pname weight ->
                     Hashtbl.replace names ((String.make 1 c)^pname) self;
                     TreeTbl.replace weights self weight
                 )
      )
    in aux ()
  in explore Nil;
  names, weights

let rec way_to_root node =
  match node with
  | Nil -> []
  | Node p -> node :: (way_to_root p)

let weight_to_root weights node =
  let wn = way_to_root node in
  let sum = ref 0 in
  List.map (
    fun i ->
      let w = TreeTbl.find weights i in
      sum := !sum + w;
      !sum
  ) wn |> List.rev

let distance weights a b =
  let wa = way_to_root a |> List.rev |> Array.of_list in
  let wb = way_to_root b |> List.rev |> Array.of_list in
  let na = Array.length wa in
  let nb = Array.length wb in
  let wga = weight_to_root weights a |> Array.of_list in
  let wgb = weight_to_root weights b |> Array.of_list in
  let j = ref 0 in
  while !j < (min na nb) && wa.(!j) == wb.(!j) do 
    incr j
  done;
  wga.(!j) + wgb.(!j)

let main s =
  List.iter (
    fun (tree, a, b) ->
      let names, weights = build tree in
      Printf.printf "%d "
      (distance weights (Hashtbl.find names a) (Hashtbl.find names b))
  ) s;
  print_newline ()


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = parse_file Sys.argv.(1) in
  main input
