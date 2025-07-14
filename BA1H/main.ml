let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let hamm s1 s2 =
  let res = ref 0 in
  String.iteri (
    fun i _c ->
      if s1.[i] <> s2.[i] then incr res
  ) s1;
  !res

let asubs pattern strand d =
  let k = String.length pattern in
  let res = ref [] in
  for i = 0 to String.length strand - k do
    if hamm pattern (String.sub strand i k) <= d then
      res := i :: !res
  done;
  List.rev !res

let main s =
  let res = Scanf.sscanf s "%s\n%s\n%d" asubs in
  List.iter (Printf.printf "%d ") res;
  print_newline ()


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
