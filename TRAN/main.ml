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

type substitution = Transition | Transversion

let transition = function
  | 'A' -> 'G'
  | 'C' -> 'T'
  | 'G' -> 'A'
  | 'T' -> 'C'
  | _ -> failwith "Symbol unexpected from a DNA strand"

let substitution_type c1 c2 =
  if c1 = transition c2 then 
    Transition
  else 
    Transversion

let count_substitutions s1 s2 =
  let tst = ref 0 in
  let tsv = ref 0 in
  for i = 0 to String.length s1 - 1 do
    let c1 = s1.[i] in
    let c2 = s2.[i] in
    if c1 <> c2 then
      match substitution_type c1 c2 with
      | Transition -> incr tst
      | Transversion -> incr tsv
  done;
  !tst, !tsv

let main s =
  let s1 = s |> List.hd |> snd in
  let s2 = s |> List.tl |> List.hd |> snd in
  let tst, tsv = count_substitutions s1 s2 in
  Printf.printf "%.11f\n" (float_of_int tst /. float_of_int tsv)




let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
