let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let build patterns =
  let nodes = Hashtbl.create 1 in
  Hashtbl.add nodes "" 1;
  let rec aux i pref words =
    match words with
    | [] -> ()
    | w :: t ->
      if w = pref then
        aux i pref t
      else if String.starts_with ~prefix:pref w then begin
        let next = w.[String.length pref] in
        Printf.printf "%d %d %c\n"
          (Hashtbl.find nodes pref) i next;
        Hashtbl.add nodes (pref^(String.make 1 next)) i;
        aux (succ i) (pref^(String.make 1 next)) words
      end else
        aux i (String.sub pref 0 (String.length pref - 1)) words
  in aux 2 "" (List.sort compare patterns)

let main s =
  let pat = s |> String.split_on_char '\n'
              |> List.map String.trim in
  build pat


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
