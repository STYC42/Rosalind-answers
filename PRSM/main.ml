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

let spec_of_prot mi p =
  let res = Array.make (String.length p) 0. in
  res.(0) <- Hashtbl.find mi p.[0];
  for i = 1 to pred (String.length p) do
    res.(i) <- res.(pred i) +. Hashtbl.find mi p.[i]
  done;
  res

let round x = floor (x +. 0.5)
let round_dfrac d x =
  if x -. (round x) = 0. then x else
  let m = 10. ** (float d) in
  (floor ((x *. m) +. 0.5)) /. m

let max_multiplicity s1 s2 =
  let h = Hashtbl.create 1 in
  Array.iter (
    fun x ->
      Array.iter (
        fun y ->
          Hashtbl.add h (round_dfrac 5 (x-.y)) 0
      ) s1
  ) s2;
  let max = ref 0 in
  Hashtbl.iter (
    fun k _v ->
      let n = List.length (Hashtbl.find_all h k) in
      if n > !max then begin
        max := n
      end
  ) h;
  !max

let max_list l =
  let max = ref 0 in
  let imax = ref (-1) in
  List.iteri (
    fun i x ->
      if x > !max then begin
        max := x;
        imax := i
      end
  ) l;
  !max, !imax

let main s =
  let mi = monoiso () in
  let ic = Scanf.Scanning.from_string s in
  let n = Scanf.bscanf ic "%d" (fun a -> a) in
  let prots = List.init n 
              (fun _i -> 
                Scanf.bscanf ic " %s" (fun a -> a)
              ) in
  let spec_prots = List.map (spec_of_prot mi) prots in
  let spec_unknown = Scanf.bscanf ic "%[^\000]" (fun s -> s)
                |> String.trim
                |> String.split_on_char '\n'
                |> List.map String.trim
                |> List.map float_of_string
                |> Array.of_list in
  let max, imax = spec_prots |> List.map (max_multiplicity spec_unknown)
                             |> max_list in
  Printf.printf "%d\n%s\n" max (List.nth prots imax)
  
  


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
