let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let gc_content s =
  let c = String.fold_left
  (fun acc c ->
    if c = 'G' || c = 'C' then acc + 1 else acc
  ) 0 s in
  float_of_int c /. float_of_int (String.length s)

let main s =
  let lines = s |> String.split_on_char '\n' in
  let strand = List.nth lines 0 in
  let n = String.length strand in
  let a = List.nth lines 1 
          |> String.split_on_char ' '
          |> List.map float_of_string in
  let e = List.map (fun x -> log10 (x/.2.)) a in
  let f = List.map (fun x -> log10 ((1.-.x)/.2.)) a in
  let g = gc_content strand in
  let res =
    List.map2 (
      fun x y -> (float_of_int n) *. (g *. x +. (1.-.g) *. y )
    ) e f in
  List.iter (Printf.printf "%.3f ") res;
  print_newline ()



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
