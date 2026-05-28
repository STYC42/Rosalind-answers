let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let rec merge a b = match a, b with
        | [], b -> b
        | a, [] -> a
        | ha::ta, hb::_ when ha < hb -> ha :: merge ta b
        | _, hb::tb -> hb :: merge a tb

let main s =
  let ic = Scanf.Scanning.from_string s in
  let n = read_int ic in
  let a = List.init n (fun _ -> read_int ic) in
  let m = read_int ic in
  let b = List.init m (fun _ -> read_int ic) in
  List.iter (Printf.printf "%d ") (merge a b);
  print_newline ()


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
