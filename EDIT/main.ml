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

let edit s1 s2 =
  let n = String.length s1 in
  let m = String.length s2 in
  let dp = Array.make_matrix (n+1) (m+1) 0 in
  for i = 1 to n do
          dp.(i).(0) <- i
  done;
  for j = 1 to m do
          dp.(0).(j) <- j
  done;

  for i = 1 to n do
    for j = 1 to m do
      if s1.[i-1] = s2.[j-1] then
        dp.(i).(j) <- dp.(i-1).(j-1)
      else
        dp.(i).(j) <- 1 + min (min dp.(i-1).(j) dp.(i).(j-1)) dp.(i-1).(j-1)
    done
  done;
  dp.(n).(m)

let main f =
        let s1 = f |> List.hd |> snd in
        let s2 = f |> List.tl |> List.hd |> snd in
        Printf.printf "%d\n" (edit s1 s2)

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
