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

let letter_to_digit =
  function
  | 'A' -> 0
  | 'C' -> 1
  | 'G' -> 2
  | 'T' -> 3
  | _ -> raise (Invalid_argument "Letter not defined")

let rec _number_to_pattern n =
  if n = 0 then 
    "" 
  else
    let q, r = n/4, n mod 4 in
    _number_to_pattern q ^ (String.make 1 (digit_to_letter r))

let pattern_to_number p =
  String.fold_left (
    fun acc c ->
      acc * 4 + letter_to_digit c
  ) 0 p

let freq_table strand k =
  let res = Array.make (1 lsl (2*k)) 0 in
  let pattern_init = String.sub strand 0 k in
  let number_init = pattern_to_number pattern_init in
  res.(number_init) <- res.(number_init) + 1;
  let current_number = ref number_init in
  for i = k to String.length strand - 1 do
    current_number := !current_number*4 - (letter_to_digit strand.[i-k])*(1 lsl (2*k)) + letter_to_digit strand.[i];
    res.(!current_number) <- res.(!current_number) + 1
  done;
  res

let main s =
  let ft = Scanf.sscanf s "%s\n%d" freq_table in
  Array.iter (Printf.printf "%d ") ft;
  print_newline ()

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
