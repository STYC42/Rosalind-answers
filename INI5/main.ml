let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let main s =
  let lines = s |> String.split_on_char '\n'
                |> List.map String.trim
                |> Array.of_list in
  Array.iteri (
    fun i l ->
      if i mod 2 = 1 then print_endline l
  ) lines



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
