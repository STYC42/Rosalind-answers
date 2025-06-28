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
  List.iter (fun (k, v) -> Hashtbl.add res k v) s;
  res

let analyze_strands s =
  let prf = Hashtbl.create 1 in
  List.iter (
    fun (k, v) ->
      String.iteri (
        fun i _ ->
          Hashtbl.add prf (String.sub v 0 i) k
      ) v
  ) s;
  prf

let build_string s =
  let f = fasta_ht s in
  let prf = analyze_strands s in
  let con = Hashtbl.create 1 in
  let rcon = Hashtbl.create 1 in
  let icon = Hashtbl.create 1 in
  List.iter (
    fun (k, v) ->
      let h = String.length v in
      let m = h / 2 in
      for i = m to h do
        match Hashtbl.find_opt prf (String.sub v (h-i) i) with
        | None -> ()
        | Some a -> begin
          Hashtbl.add con k a;
          Hashtbl.add rcon a k;
          Hashtbl.add icon k i
        end
      done
  ) s;
  let last = ref "" in
  List.iter (
    fun (k, _) ->
      match Hashtbl.find_opt con k with
      | Some _ -> ()
      | None -> last := k
  ) s;
  let res = ref [Hashtbl.find f !last] in
  for _ = 2 to List.length s do
    last := Hashtbl.find rcon !last;
    let v = Hashtbl.find f !last in
    let nv = String.length v in
    res := (String.sub v 0 (nv - (Hashtbl.find icon !last))) :: !res
  done;
  String.concat "" !res
  

let main s =
  Printf.printf "%s\n" (build_string s)

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
