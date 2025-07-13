let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let most_frequent_kmer strand k =
  let count = Hashtbl.create 1 in
  for i = 0 to String.length strand - k do
    let mer = String.sub strand i k in
    match Hashtbl.find_opt count mer with
    | None -> Hashtbl.add count mer 1
    | Some n -> Hashtbl.replace count mer (n+1)
  done;
  let max = ref 0 in
  let lmax = ref [] in
  Hashtbl.iter (
    fun mer f ->
      if f > !max then begin
        max := f;
        lmax := [mer]
      end else if f = !max then
        lmax := mer :: !lmax;
  ) count;
  !lmax


let main s =
  let res = Scanf.sscanf s "%s\n%d" most_frequent_kmer in
  List.iter (Printf.printf "%s ") res;
  print_newline ()



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
