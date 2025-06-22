let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s



let main s =
  let hmd, ht, hmr = Scanf.sscanf s "%d %d %d" (fun a b c -> (a, b, c)) in
  let n = 4 * hmd * (hmd + 2*ht + 2*hmr - 1) + ht * (3*ht + 4*hmr - 3) in
  let d = 4 * (hmd + ht + hmr) * (hmd + ht + hmr - 1) in
  let res = (float_of_int n) /. (float_of_int d) in
  Printf.printf "%.5f\n" res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
