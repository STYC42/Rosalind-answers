let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s


let main s =
  let v1, v2, v3, v4, v5, v6 = 
    Scanf.sscanf s "%d %d %d %d %d %d" (fun a b c d e f -> (a, b, c, d, e, f)) in
  Printf.printf "%.1f\n" (
    2.00 *. float_of_int v1 +.
    2.00 *. float_of_int v2 +.
    2.00 *. float_of_int v3 +.
    1.50 *. float_of_int v4 +.
    1.00 *. float_of_int v5 +.
    0.00 *. float_of_int v6
  )



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
