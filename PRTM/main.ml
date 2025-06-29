let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let monoiso () =
  let ic = Scanf.Scanning.open_in "monoisotopicMass.table" in
  let res = Hashtbl.create 1 in
  try
    while true do
      let c, w = Scanf.bscanf ic "%c %f\n" (fun a b -> (a, b)) in
      Hashtbl.add res c w
    done
  with End_of_file -> res

let main s =
  let mi = monoiso () in
  let res = ref 0. in
  String.iter (
    fun c ->
      res := !res +. Hashtbl.find mi c
  ) s;
  Printf.printf "%.3f\n" !res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
