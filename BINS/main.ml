let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let bins a n i =
        let rec aux l r =
                if l = r then
                        if a.(l) = i then l+1
                        else -1
                else
                        let mid = (l+r)/2 in
                        if a.(mid) < i then aux (mid+1) r
                        else if a.(mid) > i then aux l (mid)
                        else mid+1
        in aux 0 n

let main s =
  let ic = Scanf.Scanning.from_string s in
  let n = Scanf.bscanf ic "%d" (fun i -> i) in
  let m = Scanf.bscanf ic " %d" (fun i -> i) in
  let a = Array.init n (fun _ -> Scanf.bscanf ic " %d" (fun j -> j)) in
  let k = Array.init m (fun _ -> Scanf.bscanf ic " %d" (fun j -> j)) in
  Array.iter (fun ki ->
          Printf.printf "%d " (bins a n ki)
  ) k;
  print_newline ()


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
