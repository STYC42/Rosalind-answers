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
  let strand = s |> List.hd |> snd in
  let res = Array.make (String.length strand) 0 in
  let run = ref [-1] in
  for i = 1 to pred (String.length strand) do
    run := List.filter (
             fun j ->
              strand.[i] = strand.[j]
           ) (List.map (fun x -> x + 1) !run)
           @ [-1];
    res.(i) <- List.hd !run + 1
  done;
  Array.iter (Printf.printf "%d ") res;
  print_newline ()


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
