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

let find_sseq mot str =
  let str_seq = (String.to_seq str) () in
  let mot_seq = (String.to_seq mot) () in
  let rec aux m l i =
    match m with
    | Seq.Nil -> []
    | Seq.Cons (h, t) ->
      match l with
      | Seq.Nil -> raise Not_found
      | Seq.Cons (c, d) when c = h -> 
          (i+1) :: aux (t ()) (d ()) (succ i)
      | Seq.Cons (_c, d) -> 
          aux m (d ()) (succ i)
  in aux mot_seq str_seq 0


let main s =
  let string = s |> List.hd 
                 |> snd in
  let motif = s |> List.tl
                |> List.hd
                |> snd in
  find_sseq motif string
    |> List.iter (Printf.printf "%d ");
  print_newline ()


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
