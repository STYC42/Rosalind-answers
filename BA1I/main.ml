module StringSet = Set.Make(String)

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

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

let main s =
  let strand, k, d = Scanf.sscanf s "%s\n%d %d" (fun a b c -> a, b, c) in
  let possibl = candidates strand k d in
  let fmax = ref 0 in
  let lmax = ref [] in
  StringSet.iter (
    fun mer ->
      let f = List.length (asubs mer strand d) in
      if f > !fmax then begin
        fmax := f;
        lmax := [mer]
      end else if f = !fmax then
        lmax := mer :: !lmax
  ) possibl;
  List.iter (Printf.printf "%s ") !lmax;
  print_newline ()  

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
