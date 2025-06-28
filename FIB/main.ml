let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s


let main s =
  let n, k = Scanf.sscanf s "%d %d" (fun a b -> (a, b)) in
  let repr, aliv = ref 1, ref 1 in
  for _ = 3 to n do
    let tmp = !aliv + k * !repr in
    repr := !aliv;
    aliv := tmp
  done;
  Printf.printf "%d\n" !aliv


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
