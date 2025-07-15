let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let letter_to_digit =
  function
  | 'A' -> 0
  | 'C' -> 1
  | 'G' -> 2
  | 'T' -> 3
  | _ -> raise (Invalid_argument "Letter not defined")
  
let pattern_to_number p =
  String.fold_left (
    fun acc c ->
      acc * 4 + letter_to_digit c
  ) 0 p


let main s =
  Printf.printf "%d\n" (pattern_to_number s)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
