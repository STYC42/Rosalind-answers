let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let appr () =
  let con = Hashtbl.create 1 in
  let ic = Scanf.Scanning.open_in "RNAcodon.table" in
  try
    while true do
      let r, p = Scanf.bscanf ic "%s %s\n" (fun a b -> (a, b)) in
      Hashtbl.add con p r
    done
  with End_of_file -> begin
    let res = Hashtbl.create 1 in
    Hashtbl.iter (
      fun k _ ->
        Hashtbl.replace res k (Hashtbl.find_all con k 
                               |> List.length)
    ) con;
    res
  end

let (%*) a b =
  (a * b) mod 1000000

let main s =
  let a = appr () in
  let res = ref (Hashtbl.find a "Stop") in
  String.iter (
    fun c ->
      let sc = String.make 1 c in
      res := !res %* (Hashtbl.find a sc)
  ) s;
  Printf.printf "%d\n" !res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
