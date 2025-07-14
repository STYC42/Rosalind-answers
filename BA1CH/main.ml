module StringSet = Set.Make(String)

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

let complement = function 
  | 'A' -> 'T'
  | 'C' -> 'G'
  | 'G' -> 'C'
  | 'T' -> 'A'
  | _ -> failwith "character unexpected from a DNA strand"

let revc strand =
  let l = String.length strand in
  String.init l (
    fun i -> complement strand.[l - i - 1]
  )

let sum =
  List.fold_left (+) 0

let average l =
  sum l / List.length l


let min_skew strand =
  let skew = ref 0 in
  let vmin = ref max_int in
  let lmin = ref [] in
  String.iteri (
    fun i ->
      function
      | 'A' -> ()
      | 'C' -> decr skew;
              if !skew < !vmin then begin
                vmin := !skew;
                lmin := [i+1]
              end else if !skew = !vmin then
                lmin := (i+1) :: !lmin
      | 'G' -> incr skew
      | 'T' -> ()
      | _ -> failwith "character unexpected from a DNA strand"
  ) strand;
  !lmin

let rec d_neigh d mer =
  if d = 0 then StringSet.singleton mer else begin
  let res = ref (d_neigh (d-1) mer) in
  for i = 0 to String.length mer -1 do
    let before = String.sub mer 0 i in
    let after = String.sub mer (i+1) (String.length mer - i - 1) in
    List.iter (
      fun c -> res := StringSet.union !res (d_neigh (d-1) (before^c^after))
    ) ["A"; "C"; "G"; "T"]
  done;
  !res end

let candidates strand k d = 
  let res = ref StringSet.empty in
  for i = 0 to String.length strand - k do
    let sub = String.sub strand i k in
    res := StringSet.union !res (d_neigh d sub)
  done;
  !res

let hamm s1 s2 =
  let res = ref 0 in
  String.iteri (
    fun i _c ->
      if s1.[i] <> s2.[i] then incr res
  ) s1;
  !res

let asubs pattern strand d =
  let k = String.length pattern in
  let res = ref [] in
  for i = 0 to String.length strand - k do
    if hamm pattern (String.sub strand i k) <= d then
      res := i :: !res
  done;
  List.rev !res

let most_frequent strand k d =
  let possibl = candidates strand k d in
  let fmax = ref 0 in
  let lmax = ref [] in
  StringSet.iter (
    fun mer ->
      let f = List.length (asubs mer strand d) + List.length (asubs (revc mer) strand d) in
      if f > !fmax then begin
        fmax := f;
        lmax := [mer]
      end else if f = !fmax then
        lmax := mer :: !lmax
  ) possibl;
  !lmax, !fmax

let main s =
  let strand = s |> List.hd |> snd in
  let ms = min_skew strand in
  let avr = average ms in
  let window = String.sub strand (avr-500) 1000 in
  let mf, f = most_frequent window 9 1 in
  Printf.fprintf stderr "%d\n" f;
  List.iter (fun s -> prerr_endline (s^"|"^(revc s))) mf


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_fasta Sys.argv.(1) in
  main input
