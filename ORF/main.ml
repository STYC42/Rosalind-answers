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
    
  
let revc s =
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
    

let dna2rna s =
  (String.map
    (function
      | 'A' -> 'A'
      | 'C' -> 'C'
      | 'G' -> 'G'
      | 'T' -> 'U'
      | c -> Printf.printf "Symbol unexpected from a DNA string : %c\n" c;
            exit 1
      ) s
  )

let codon_table () =
  let ic = Scanf.Scanning.open_in "RNAcodon.table" in
  let ht = Hashtbl.create 64 in
  for _ = 1 to 64 do
    let k, v = Scanf.bscanf ic " %s %s" (fun a b -> (a, b)) in
    Hashtbl.add ht k v
  done;
  ht

let rna2prot s =
  let ts = codon_table () in
  let res = ref [] in
  for i = 0 to (String.length s) / 3 - 1 do
    let w = String.sub s (i*3) 3 in
    res := (match Hashtbl.find ts w with
            | "Stop" -> "#"
            | c -> c) :: !res
  done;
  String.concat "" (List.rev !res)

let candidates s =
  let res = ref [] in
  let subs = String.split_on_char '#' s in
  let k = List.length subs in
  List.iteri (
    fun i str ->
      if i <> pred k then begin
        let n = String.length str in
        String.iteri (
          fun i c ->
            if c = 'M' then
              res := String.sub str i (n-i) :: !res
        ) str
      end
  ) subs;
  !res

let main s =
  let res = Hashtbl.create 1 in
  let str = s |> List.hd |> snd in
  let revstr = revc str in
  for i = 0 to 2 do
    let rf_sp = String.sub str i (String.length str - i) in
    let rf_rv = String.sub revstr i (String.length revstr - i) in
    List.iter (
      fun cand -> Hashtbl.replace res cand ()
    ) (rf_sp |> dna2rna |> rna2prot |> candidates);
    List.iter (
      fun cand -> Hashtbl.replace res cand ()
    ) (rf_rv |> dna2rna |> rna2prot |> candidates)
  done;
  Hashtbl.iter (
    fun k _v ->
      print_endline k
  ) res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
