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

let hash str =
  String.fold_left (
    fun acc c -> 
      4*acc + 
      match c with 
      |'A' -> 0 
      | 'C' -> 1 
      | 'G' -> 2 
      | 'T' -> 3 
      | _ -> failwith "Not a DNA strand"
  ) 0 str

let main s =
  let str = s |> List.hd |> snd in
  let res = Array.make 256 0 in
  for i = 0 to String.length str - 4 do
    let spl = String.sub str i 4 in
    res.(hash spl) <- res.(hash spl) + 1
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
