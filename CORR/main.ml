let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

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

let fasta_ht s =
  let res = Hashtbl.create 1 in
  List.iter (
    fun (k, v) ->
      Hashtbl.add res k v
  ) s;
  res

let fasta_revht s =
  let res = Hashtbl.create 1 in
  List.iter (
    fun (k, v) ->
      Hashtbl.add res v k
  ) s;
  res

let hamm s t =
  let c = ref 0 in
  for i = 0 to pred (String.length s) do
    if s.[i] <> t.[i] then incr c
  done;
  !c

let reverse_string s =
  let len = String.length s in
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i s.[len - 1 - i]
  done;
  Bytes.to_string res

let reverse s =
  reverse_string
    (String.map
      (function
        | 'A' -> 'T'
        | 'C' -> 'G'
        | 'G' -> 'C'
        | 'T' -> 'A'
        | c -> Printf.printf "Symbol unexpected from a DNA string : %c\n" c;
                exit 1
      ) s
    )

let values s =
  let strings = Hashtbl.create 1 in
  List.iter (
    fun (k, v) ->
      Hashtbl.add strings v k;
      Hashtbl.add strings (reverse v) k
  ) s;
  strings

let valids strings =
  let res = ref [] in
  Hashtbl.iter (
    fun s c ->
      let fa = Hashtbl.find_all strings s in
      if List.length fa >= 2 then res := s :: !res
  ) strings;
  Array.of_list !res

let not_valids strings =
  let res = ref [] in
  Hashtbl.iter (
    fun s c ->
      let fa = Hashtbl.find_all strings s in
      if List.length fa = 1 then res := s :: !res
  ) strings;
  Array.of_list !res

let main s =
  let fr = fasta_revht s in
  let strings = values s in
  let vals = valids strings in
  let nvals = not_valids strings in
  let changes = Hashtbl.create 1 in
  Array.iter (
    fun nv ->
      Array.iter (
        fun v ->
          if hamm v nv = 1 then Hashtbl.add changes nv v
      ) vals
  ) nvals;
  Hashtbl.iter (
    fun s c ->
      match Hashtbl.find_opt changes s with
      | Some a -> Printf.printf "%s->%s\n" s a
      | None -> ()
  ) fr



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
