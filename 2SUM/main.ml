exception Found of int*int

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)


let main s =
  let ic = Scanf.Scanning.from_string s in
  let k = read_int ic in
  let n = read_int ic in
  for _=0 to k-1 do
        let a = Array.init n (fun _ -> read_int ic) in
        let h = Hashtbl.create 1 in
        try Array.iteri (fun i x ->
                match Hashtbl.find_opt h x with
                | None -> Hashtbl.add h (-x) i
                | Some j -> raise (Found (i, j))) a; print_endline "-1"
        with Found (i, j) -> Printf.printf "%d %d\n" (j+1) (i+1)
  done


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
