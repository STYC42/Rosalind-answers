let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let rec merge a b = match a, b with
        | [], b -> b
        | a, [] -> a
        | ha::ta, hb::_ when ha < hb -> ha :: merge ta b
        | _, hb::tb -> hb :: merge a tb

let rec split = function
        | [] -> ([], [])
        | h :: t -> let a, b = split t in (h::b, a)

let rec sort = function
        | [] -> []
        | [h] -> [h]
        | l -> let a, b = split l in
                let a' = sort a in
                let b' = sort b in
                merge a' b'

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let main s =
  let ic = Scanf.Scanning.from_string s in
  let n = read_int ic in
  let l = List.init n (fun _ -> read_int ic) in
  l |> sort |> List.iter (Printf.printf "%d ")


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
