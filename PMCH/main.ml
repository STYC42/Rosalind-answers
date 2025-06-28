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

let rec fact k =
  if k = 0 then Z.one else Z.mul (Z.of_int k) (fact (k - 1))


let main s =
  let str = s |> List.hd |> snd in
  let nb_a = String.fold_left 
            (fun acc c -> 
              if c = 'A' then succ acc else acc
            ) 0 str in
  let nb_t = (String.length str / 2) - nb_a in
  Printf.printf "%s\n" (Z.mul (fact nb_a) (fact nb_t) |> Z.to_string)



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
