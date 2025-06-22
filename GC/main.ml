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

let gc_content (k, s) =
  let c = String.fold_left
          (fun acc c ->
            if c = 'G' || c = 'C' then acc + 1 else acc
          ) 0 s in
  (k, 100. *. float_of_int c /. float_of_int (String.length s))

let main lf =
  let res = 
    List.fold_left
    (fun acc b ->
      let g = gc_content b in
      if snd acc < snd g then g else acc
    ) ("", 0.) lf in
  Printf.printf "%s\n%.6f\n" (fst res) (snd res)



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
