let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let connectable a b =
  (a = 'A' && b = 'U') ||
  (a = 'U' && b = 'A') ||
  (a = 'G' && b = 'C') ||
  (a = 'C' && b = 'G') ||
  (a = 'U' && b = 'G') ||
  (a = 'G' && b = 'U')

let main s =
  let str = s |> String.trim in
  let n = String.length str in
  let dyn = Array.make_matrix (n+1) (n+1) Z.minus_one in
  let rec aux i j =
    assert (0 <= i);
    assert (i <= j);
    assert (j <= n);
    if j-i = 0 then dyn.(i).(j) <- Z.one else
    if j-i = 1 then dyn.(i).(j) <- Z.one else
    if dyn.(i).(j) = Z.minus_one then begin
      let res = ref Z.zero in
      for k = i+5 to j do
        if connectable str.[i] str.[pred k] then begin
          aux (succ i) (pred k);
          aux k j;
          res := Z.add !res (Z.mul dyn.(succ i).(pred k) dyn.(k).(j))
        end
      done;
      aux (succ i) j;
      res := Z.add !res dyn.(succ i).(j);
      dyn.(i).(j) <- !res
    end;
    
  in aux 0 n;
  Printf.printf "%s\n" (Z.to_string dyn.(0).(n))

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
