let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let binom n =
  let dyn = Array.make_matrix (n+1) (n+1) (-1) in
  let rec aux m j =
    if dyn.(m).(j) <> -1 then () 
    else if m = 0 then dyn.(m).(j) <- 0
    else if j > m then dyn.(m).(j) <- 0 
    else if j = 0 then dyn.(m).(j) <- 1
    else if j = m then dyn.(m).(j) <- 1
    else begin
      aux (m-1) j;
      aux (m-1) (j-1);
      dyn.(m).(j) <- dyn.(m-1).(j) + dyn.(m-1).(j-1)
    end
  in
  for i = 0 to n do
    aux n i
  done;
  dyn.(n)

let main s =
  let k, n = Scanf.sscanf s "%d %d" (fun a b -> (a, b)) in
  let b = binom (1 lsl k) in
  let res = ref 0. in
  for i = n to 1 lsl k do
    res := !res +. (float_of_int b.(i) *. (3. ** float_of_int ((1 lsl k) - i))) 
  done;
  res := !res /. (4. ** float_of_int (1 lsl k));
  Printf.printf "%.3f\n" !res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
