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

let (%*) a b =
  (a*b) mod 1000000

let (%+) a b =
  (a+b) mod 1000000

let connectable a b =
  (a = 'A' && b = 'U') ||
  (a = 'U' && b = 'A') ||
  (a = 'G' && b = 'C') ||
  (a = 'C' && b = 'G')

let main s =
  let str = s |> List.hd |> snd in
  let n = String.length str in
  let dyn = Array.make_matrix (n+1) (n+1) (-1) in
  let rec aux i j =
    assert (0 <= i);
    assert (i <= j);
    assert (j <= n);
    if j-i = 0 then dyn.(i).(j) <- 1 else
    if j-i = 1 then dyn.(i).(j) <- 1 else
    if dyn.(i).(j) = -1 then begin
      let res = ref 0 in
      for k = i+2 to j do
        if connectable str.[i] str.[pred k] then begin
          aux (succ i) (pred k);
          aux k j;
          res := !res %+ (dyn.(succ i).(pred k) %* dyn.(k).(j))
        end
      done;
      aux (succ i) j;
      res := !res %+ dyn.(succ i).(j);
      dyn.(i).(j) <- !res
    end;
    
  in aux 0 n;
  Printf.printf "%d\n" dyn.(0).(n)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
