let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let rec part p = function
        | [] -> ([], [], [])
        | h::t -> let a, b, c = part p t in
                if h < p then (h::a, b, c) 
                else if h > p then (a, b, h::c)
                else (a, h::b, c)

let main s =
  let ic = Scanf.Scanning.from_string s in
  let n = read_int ic in
  let l = List.init n (fun _ -> read_int ic) in
  let a, b, c = part (List.hd l) l in
  a @ b @ c |> List.iter (Printf.printf "%d ")


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
