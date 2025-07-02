let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let monoiso_rev () =
  let ic = Scanf.Scanning.open_in "monoisotopicMass.table" in
  let res = Hashtbl.create 1 in
  try
    while true do
      let c, w = Scanf.bscanf ic "%c %f\n" (fun a b -> (a, b)) in
      Hashtbl.add res w c
    done
  with End_of_file -> res

let spectr mnr =
  let res = mnr |> Hashtbl.to_seq_keys
                |> Array.of_seq in
  Array.sort compare res;
  res

let near_amin mnr sp f =
  let j = ref 0 in
  while !j < Array.length sp && sp.(!j) < f do incr j done;
  Hashtbl.find mnr
    sp.(if !j = 0 then 0 else
        if !j = Array.length sp then pred (Array.length sp) else
          let bef = f -. sp.(pred !j) in
          let aft = sp.(!j) -. f in
          if bef < aft then pred !j else !j)


let rec diff l =
  match l with
  | [] -> []
  | _h :: [] -> []
  | h1 :: h2 :: t -> (h2 -. h1) :: diff (h2 :: t)
  
let main s =
  let l = s |> String.split_on_char '\n'
            |> List.map String.trim
            |> List.map float_of_string
            |> diff in
  let mnr = monoiso_rev () in
  let sp = spectr mnr in
  List.iter (
    fun d ->
      print_char (near_amin mnr sp d)
  ) l;
  print_newline ()
  


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
