let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let round x = floor (x +. 0.5)
let round_dfrac d x =
  if x -. (round x) = 0. then x else
  let m = 10. ** (float d) in
  (floor ((x *. m) +. 0.5)) /. m

let main s =
  let lines = String.split_on_char '\n' s in
  let s1 = List.nth lines 0
           |> String.split_on_char ' '
           |> List.map String.trim
           |> List.map float_of_string in
  let s2 = List.nth lines 1
           |> String.split_on_char ' '
           |> List.map String.trim
           |> List.map float_of_string in
  let h = Hashtbl.create 1 in
  List.iter (
    fun x ->
      List.iter (
        fun y ->
          Hashtbl.add h (round_dfrac 5 (x-.y)) 0
      ) s1
  ) s2;
  let max = ref 0 in
  let vmax = ref (-1.) in
  Hashtbl.iter (
    fun k _v ->
      let n = List.length (Hashtbl.find_all h k) in
      if n > !max then begin
        max := n;
        vmax := k
      end
  ) h;
  Printf.printf "%d\n%.5f\n" !max (abs_float !vmax)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
