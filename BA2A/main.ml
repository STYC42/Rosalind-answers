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

let candidates k d strand= 
  let res = ref StringSet.empty in
  for i = 0 to String.length strand - k do
    let sub = String.sub strand i k in
    res := StringSet.union !res (d_neigh d sub)
  done;
  !res

let kd_motifs strands k d =
  let each = List.map (candidates k d) strands in
  List.fold_left StringSet.inter (List.hd each) (List.tl each)

let main s =
  let lines = s |> String.split_on_char '\n'
                |> List.map String.trim in
  let k, d = Scanf.sscanf (List.hd lines) "%d %d" (fun a b -> a, b) in
  let strands = List.tl lines in
  let res = kd_motifs strands k d in
  if StringSet.is_empty res then
    Printf.printf "nan \n"
  else begin
    StringSet.iter (Printf.printf "%s ") res;
    print_newline ()
  end



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
