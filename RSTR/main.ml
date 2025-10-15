let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let gc_content s =
  let c = String.fold_left
  (fun acc c ->
    if c = 'G' || c = 'C' then acc + 1 else acc
  ) 0 s in
  float_of_int c /. float_of_int (String.length s)

let lprob strand gc =
  let n = String.length strand in
  let g = gc_content strand in
  let lgc = log10 (gc/.2.) in
  let lmgc = log10 ((1.-.gc)/.2.) in
  (float_of_int n) *. (g *. lgc +. (1.-.g) *. lmgc)

let main s =
  let lines = s |> String.split_on_char '\n' 
                |> List.map String.trim in
  let strand = List.nth lines 1 in
  let n, p = Scanf.sscanf (List.hd lines) "%d %f" (fun a b -> a, b) in
  let lp = lprob strand p in
  let res = 1. -. ( (1. -. 10. ** (lp)) ** (float_of_int n) ) in
  Printf.printf "%.3f\n" res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
