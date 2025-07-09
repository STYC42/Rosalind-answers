let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let main s =
  let lines = s |> String.split_on_char '\n'
                |> List.map String.trim 
                |> Array.of_list in
  let string = lines.(0) in
  let a, b, c, d = Scanf.sscanf lines.(1) "%d %d %d %d" (fun a b c d -> (a, b, c, d)) in
  let s1 = String.sub string a (b-a+1) in
  let s2 = String.sub string c (d-c+1) in
  Printf.printf "%s %s\n" s1 s2


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
