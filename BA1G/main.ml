let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let hamm s1 s2 =
  let res = ref 0 in
  String.iteri (
    fun i _c ->
      if s1.[i] <> s2.[i] then incr res
  ) s1;
  !res

let main s =
  let res = Scanf.sscanf s "%s\n%s" hamm in
  Printf.printf "%d\n" res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
