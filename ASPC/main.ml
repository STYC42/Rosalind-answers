let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let (%+) a b =
  (a+b) mod 1000000

let binom l =
  let dyn = Array.make_matrix (l+1) (l+1) (-1) in
  let rec aux n k =
    if dyn.(n).(k) = -1 then
      dyn.(n).(k) <- if n < k then 0
                     else if n = k || k = 0 then 1
                     else aux (n-1) k %+ aux (n-1) (k-1);
    dyn.(n).(k)
  in 
  for i = 0 to l do
    ignore(aux l i)
  done;
  dyn.(l)
 
let main s =
  let n, m = Scanf.sscanf s "%d %d" (fun a b -> (a, b)) in
  let res = ref 0 in
  let comb = binom n in
  for i = m to n do
    res := !res %+ comb.(i)
  done;
  Printf.printf "%d\n" !res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
