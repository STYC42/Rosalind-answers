let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let (%*) a b =
  (a*b) mod 1000000

let rec dbl_fact k =
  if k = 1 then 1 else
  if k = 2 then 2 else
  k %* dbl_fact (k-2)

let main s =
  let n = Scanf.sscanf s "%d" (fun a -> a) in
  Printf.printf "%d\n" (dbl_fact (2*n - 5))



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
