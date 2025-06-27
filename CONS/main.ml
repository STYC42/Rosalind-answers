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
  let n = s |> List.hd |> snd |> String.length in
  let popA = Array.make n 0 in
  let popC = Array.make n 0 in
  let popG = Array.make n 0 in
  let popT = Array.make n 0 in
  List.iter (
    fun st ->
    String.iteri (
      fun i c ->
        match c with
        | 'A' -> popA.(i) <- popA.(i) + 1
        | 'C' -> popC.(i) <- popC.(i) + 1
        | 'G' -> popG.(i) <- popG.(i) + 1
        | 'T' -> popT.(i) <- popT.(i) + 1
        | c -> Printf.fprintf stderr "Caractère non reconnu : %c\n" c
    ) (snd st)
  ) s;
  for i = 0 to pred n do
    let a = popA.(i) in
    let c = popC.(i) in
    let g = popG.(i) in
    let t = popT.(i) in
    if a >= c && a >= g && a >= t then
      print_char 'A'
    else if c >= g && c >= t then
      print_char 'C'
    else if g >= t then
      print_char 'G'
    else
      print_char 'T'
  done;
  print_newline ();

  print_string "A: ";
  Array.iter (Printf.printf "%d ") popA;
  print_newline ();
  print_string "C: ";
  Array.iter (Printf.printf "%d ") popC;
  print_newline ();
  print_string "G: ";
  Array.iter (Printf.printf "%d ") popG;
  print_newline ();
  print_string "T: ";
  Array.iter (Printf.printf "%d ") popT;
  print_newline ()

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
