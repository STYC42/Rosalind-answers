let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let count strand pattern =
  let res = ref 0 in
  for i = 0 to String.length strand - String.length pattern do
    if String.sub strand i (String.length pattern) = pattern then
      incr res
  done;
  !res

let main s =
  let lines = s |> String.split_on_char '\n'
                |> List.map String.trim
                |> Array.of_list in
  let strand = lines.(0) in
  let pattern = lines.(1) in
  Printf.printf "%d\n" (count strand pattern)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
