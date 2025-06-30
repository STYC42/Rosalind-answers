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

let rec decompos a =
  if a = 0 then [] else begin
  (a land ((1 lsl 31) - 1)) :: decompos (a lsr 31)
  end

let rec sum l =
  match l with
  | [] -> 0
  | h :: t -> h + sum t

let rec calcule_mod a =
  if a >= (1 lsl 31 - 1) then begin
    a |> decompos |> sum |> calcule_mod
  end else a

let hash_prog o p s e =
  let ret = ref s in
  for _ = 1 to p - 1 do
    ret := !ret lsl 8 |> calcule_mod
  done;
  let crt = (o - !ret) in
  if crt < 0 then begin
    (((crt + (1 lsl 31 - 1)) lsl 8) + e) |> calcule_mod
  end else
    ((crt lsl 8) + e) |> calcule_mod

let hash s =
  let res = ref 0 in
  Array.iter (fun e -> res := ((!res lsl 8) + e) |> calcule_mod) s;
  calcule_mod !res

let find_subs l w =
  let n, p = String.length l, String.length w in
  let res = ref [] in
  let txt = Array.init n (fun i -> int_of_char l.[i]) in
  let wrd = Array.init p (fun i -> int_of_char w.[i]) in
  let hash_wrd = hash wrd in
  let hash_sub = Array.sub txt 0 p |> hash |> ref in
  for i = p to pred n do
    assert (hash (Array.sub txt (i-p) p) = !hash_sub);
    if !hash_sub = hash_wrd then
      if String.sub l (i-p) p = w then
        res := (i-p+1) :: !res;
    hash_sub := hash_prog !hash_sub p txt.(i-p) txt.(i)
  done;
  if !hash_sub = hash_wrd then
    if String.sub l (n-p) p = w then
      res := (n-p+1) :: !res;
  !res |> List.rev 

let dna2rna s =
  String.map
    (function
      | 'A' -> 'A'
      | 'C' -> 'C'
      | 'G' -> 'G'
      | 'T' -> 'U'
      | c -> Printf.printf "Symbol unexpected from a DNA string : %c\n" c;
            exit 1
      ) s

let codon_table () =
  let ic = Scanf.Scanning.open_in "RNAcodon.table" in
  let ht = Hashtbl.create 64 in
  for _ = 1 to 64 do
    let k, v = Scanf.bscanf ic " %s %s" (fun a b -> (a, b)) in
    Hashtbl.add ht k v
  done;
  ht

exception Stop_sequence

let rna2prot s =
  let ts = codon_table () in
  let res = ref [] in
  begin try
    for i = 0 to (String.length s) / 3 - 1 do
      let w = String.sub s (i*3) 3 in
      res := 
        (match Hashtbl.find ts w with 
        | "Stop" -> raise Stop_sequence
        | c -> c) :: !res
    done
  with Stop_sequence -> () end;
  String.concat "" (List.rev !res)

let main s =
  let string = s |> List.hd |> snd in
  let introns = s |> List.tl |> List.map snd in
  let pos_introns = introns |> List.map (find_subs string) in
  let mask = Array.make (String.length string) true in
  List.iter2 (
    fun itr pos ->
      let k = String.length itr in
      List.iter (
        fun l ->
          for i = l-1 to pred (l-1+k) do
            mask.(i) <- false
          done
      ) pos
  ) introns pos_introns;
  let res = ref [] in
  String.iteri (
    fun i c ->
      if mask.(i) then res := c :: !res
  ) string;
  print_endline (!res |> List.rev
                      |> List.map (String.make 1)
                      |> String.concat ""
                      |> dna2rna
                      |> rna2prot)
  



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
