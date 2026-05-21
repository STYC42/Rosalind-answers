let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s


let main s =
  let ic = Scanf.Scanning.from_string s in
  let n = Scanf.bscanf ic "%d" (fun i -> i) in
  let m = Scanf.bscanf ic " %d" (fun i -> i) in
  let a = Array.make n 0 in
  for _ = 0 to 2*m - 1 do
          let t = Scanf.bscanf ic " %d" (fun j -> j) in
          assert (0 < t && t <= n);
          a.(t-1) <- a.(t-1) + 1
  done;
  Array.iter (Printf.printf "%d ") a;
  print_newline ()


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
