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

let hamm s t =
  let c = ref 0 in
  for i = 0 to pred (String.length s) do
    if s.[i] <> t.[i] then incr c
  done;
  !c

let main s =
  let strings = s |> List.map snd in
  let n = strings |> List.hd |> String.length in
  List.iter (
    fun l ->
      List.iter (
        fun k ->
          Printf.printf "%.5f " 
            ((float_of_int (hamm l k)) /. float_of_int n)
      ) strings;
      print_newline ()
  ) strings


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
