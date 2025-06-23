let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s


let main s =

  let s, t = Scanf.sscanf s "%s\n%s" (fun a b -> (a, b)) in
  let c = ref 0 in
  for i = 0 to pred (String.length s) do
    if s.[i] <> t.[i] then incr c
  done;
  Printf.printf "%d\n" !c


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
