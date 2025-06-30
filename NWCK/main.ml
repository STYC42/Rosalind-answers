let split_on_double_newline s =
  Str.split (Str.regexp "\n\n") s

let parse_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let blocks = split_on_double_newline content in
  List.map (fun block ->
    match String.split_on_char '\n' block with
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

let subs s motif =
  let re = Str.regexp_string motif in
  let rec aux acc pos =
    try
      let i = Str.search_forward re s pos in
      aux (i :: acc) (i + 1)
    with Not_found ->
      List.rev acc
  in
  aux [] 0

let distance tree a b =
  let pos_a = subs tree a |> List.hd in
  let pos_b = subs tree b |> List.hd in
  let min_ab = min pos_a pos_b in
  let max_ab = max pos_a pos_b in
  let s = Stack.create () in
  let high = ref 0 in
  let virg = ref false in
  String.iter (
    fun c ->
      if c = '(' then begin
        Stack.push c s
      end else if c = ')' then begin
        if Stack.is_empty s || Stack.top s = ')' then begin
          Stack.push ')' s;
          if Stack.length s > !high then begin
            high := Stack.length s;
            virg := false
          end
        end else
          ignore (Stack.pop s)
      end else if (Stack.is_empty s || Stack.top s = ')')
                  && Stack.length s = !high
                  && c = ',' then
        virg := true
  ) (String.sub tree min_ab (max_ab - min_ab));
  Stack.length s + (if !virg then 2 else 0)


let main filename =
  let pars = parse_file filename in
  List.iter (
    fun (tree, a, b) -> 
      Printf.printf "%d " (distance tree a b); flush stdout
  ) pars;
  print_newline ()

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  main Sys.argv.(1)
