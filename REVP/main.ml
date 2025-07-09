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

  let reverse_string s =
    let len = String.length s in
    let res = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set res i s.[len - 1 - i]
    done;
    Bytes.to_string res
    
  
  let rev s =
    reverse_string
      (String.map
        (function
          | 'A' -> 'T'
          | 'C' -> 'G'
          | 'G' -> 'C'
          | 'T' -> 'A'
          | c -> Printf.fprintf stderr 
                  "Symbol unexpected from a DNA string : %c\n" c;
                  exit 1
        ) s
      )
    


let naive s =
  let res = ref [] in
  for i = 0 to String.length s - 1 do
    for l = 4 to 12 do
      if i + l <= String.length s then 
        let sub = String.sub s i l in
        if sub = rev sub then
          res := (i+1, l) :: !res
    done
  done;
  List.rev !res

let main s =
  let strand = s |> List.hd |> snd in
  let res = naive strand in
  let print_double = fun (a, b) -> Printf.printf "%d %d\n" a b in
  List.iter print_double res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
