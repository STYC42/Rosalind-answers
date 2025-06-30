let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let (%*) a b =
  (a * b) mod 1000000

let rec partp n p =
  if p = 1 then n else
  n %* (partp (pred n) (pred p))

let main s =
  Printf.printf "%d\n"
    (Scanf.sscanf s "%d %d" partp)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
