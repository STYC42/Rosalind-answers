let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let rec sum l =
  match l with
  | [] -> 0
  | h :: t -> h + sum t

let hamm s1 s2 =
  let res = ref 0 in
  String.iteri (
    fun i _c ->
      if s1.[i] <> s2.[i] then incr res
  ) s1;
  !res

let dist pattern strand =
  let n = String.length strand in
  let k = String.length pattern in
  let vmin = ref max_int in
  for i = 0 to n - k do
    let sub = String.sub strand i k in
    vmin := min !vmin (hamm sub pattern)
  done;
  !vmin

let dist_set pattern strands =
  strands |> List.map (dist pattern)
          |> sum

let nucl_of_int n =
  match n with
  | 0 -> "A"
  | 1 -> "C"
  | 2 -> "G"
  | 3 -> "T"
  | _ -> raise (Invalid_argument "Code not defined")

let rec strand_of_int n l =
  if l = 0 then "" else
  let q, r = n/4, n mod 4 in
  nucl_of_int r ^ strand_of_int q (l-1)

let median_string k strands =
  let dmin = ref max_int in
  let vmin = ref "" in
  for i = 0 to 1 lsl (2*k) do
    let s = strand_of_int i k in
    let d = dist_set s strands in
    if d < !dmin then begin
      dmin := d;
      vmin := s
    end
  done;
  !vmin


let main s =
  let lines = s |> String.split_on_char '\n' 
                |> List.map String.trim in
  let k = (lines |> List.hd
                 |> Scanf.sscanf) "%d" (fun a -> a) in
  let dna = List.tl lines in
  print_endline (median_string k dna)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
