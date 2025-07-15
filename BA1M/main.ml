let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let digit_to_letter = 
  function
  | 0 -> 'A'
  | 1 -> 'C'
  | 2 -> 'G'
  | 3 -> 'T'
  | _ -> raise (Invalid_argument "Code not defined")

let rec number_to_pattern n k =
  if k = 0 then 
    "" 
  else
    let q, r = n/4, n mod 4 in
  number_to_pattern q (k-1) ^ (String.make 1 (digit_to_letter r))


let main s =
  Printf.printf "%s\n" (Scanf.sscanf s "%d\n%d" number_to_pattern)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
