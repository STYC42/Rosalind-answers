module StringSet = Set.Make(String)

let substrings s =
  prerr_endline "substrings";
  let res = ref [] in
  for i = 0 to String.length s - 1 do
    for j = i + 1 to String.length s - 1 do
      res := String.sub s i (j-i) :: !res
    done
  done;
  StringSet.of_list !res

let read_fasta filename =
  let ic = open_in filename in
  let rec read_lines acc current_id current_seq =
    try
      let line = input_line ic in
      if String.length line > 0 && line.[0] = '>' then
        let id = String.sub line 1 (String.length line - 1) in
        let acc =
          match current_id with
          | Some id -> (id, String.concat "" (List.rev current_seq)) :: acc
          | None -> acc
        in
        read_lines acc (Some id) []
      else
        read_lines acc current_id (line :: current_seq)
    with End_of_file ->
      let acc =
        match current_id with
        | Some id -> (id, String.concat "" (List.rev current_seq)) :: acc
        | None -> acc
      in
      close_in ic;
      List.rev acc
  in
  read_lines [] None []

let main s =
  let inputs = List.map snd s in
  let all = List.fold_left (fun acc i -> StringSet.inter acc (substrings i)) (substrings (List.hd inputs)) inputs in
  let max = ref "" in
  StringSet.iter (
    fun sub ->
      if String.length sub > String.length !max then
        max := sub
  ) all;
  print_endline !max

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
