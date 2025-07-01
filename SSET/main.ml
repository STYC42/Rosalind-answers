let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let main s =
  let n = Scanf.sscanf s "%d" (fun a -> a) in
  let res = ref 1 in
  for _i = 1 to n do
    res := (!res lsl 1) mod 1000000
  done;
  Printf.printf "%d\n" !res

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
