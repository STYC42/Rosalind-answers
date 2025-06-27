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


let main s =
  let n = List.length s in
  let ip = Hashtbl.create n in
  List.iter (fun (k, v) -> Hashtbl.add ip k v) s;
  let prf = Hashtbl.create n in
  List.iter (
    fun (k, v) ->
      if String.length v >= 3 then
        Hashtbl.add prf (String.sub v 0 3) k
  ) s;
  List.iter (
    fun (k, v) ->
      let m = String.length v in
      let suf = String.sub v (m-3) 3 in
      List.iter (
        fun x ->
          if Hashtbl.find ip x <> Hashtbl.find ip k then
            Printf.printf "%s %s\n" k x
      ) (Hashtbl.find_all prf suf)
  ) s


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
