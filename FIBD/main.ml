let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s


let main s =
  let n, m = Scanf.sscanf s "%d %d" (fun a b -> (a, b)) in
  let d = Array.make (n + 1) 1 in
  for i = 2 to n - 1 do
    let aliv = d.(i) in
    let repr = d.(i-1) in
    let dead = if i - m < 0 then 0 else d.(i - m) in
    d.(i + 1) <- aliv + repr - dead
  done;
  Printf.printf "%d\n" d.(n)

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
