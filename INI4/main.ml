let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let main s =
  let a, b = Scanf.sscanf s "%d %d" (fun a b -> (a, b)) in
  let c, d = ((a+1)/2)*2, (b/2)*2 in
  let res = (c+d)*(d-c)/4 in
  Printf.printf "%d\n" res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
