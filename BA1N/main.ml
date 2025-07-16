module StringSet = Set.Make(String)

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let rec d_neigh mer d =
  if d = 0 then StringSet.singleton mer else begin
  let res = ref (d_neigh mer (d-1)) in
  for i = 0 to String.length mer -1 do
    let before = String.sub mer 0 i in
    let after = String.sub mer (i+1) (String.length mer - i - 1) in
    List.iter (
      fun c -> res := StringSet.union !res (d_neigh (before^c^after) (d-1))
    ) ["A"; "C"; "G"; "T"]
  done;
  !res end

let main s =
  let res = Scanf.sscanf s "%s\n%d" d_neigh in
  StringSet.iter print_endline res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
