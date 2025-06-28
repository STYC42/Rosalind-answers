let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let codon_table () =
  let ic = Scanf.Scanning.open_in "RNAcodon.table" in
  let ht = Hashtbl.create 64 in
  for _ = 1 to 64 do
    let k, v = Scanf.bscanf ic " %s %s" (fun a b -> (a, b)) in
    Hashtbl.add ht k v
  done;
  ht

exception Stop_sequence

let main s =
  let ts = codon_table () in
  let res = ref [] in
  begin try
    for i = 0 to (String.length s) / 3 - 1 do
      let w = String.sub s (i*3) 3 in
      res := 
        (match Hashtbl.find ts w with 
        | "Stop" -> raise Stop_sequence
        | c -> c) :: !res
    done
  with Stop_sequence -> () end;
  Printf.printf "%s\n" (String.concat "" (List.rev !res))


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
