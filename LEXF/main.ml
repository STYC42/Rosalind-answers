let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let rec words alph poss n =
  if n = 0 then [""] else
  match poss with
  | [] -> []
  | h :: t ->
    (List.map (
       fun s -> h ^ s
     ) (words alph alph (n-1)))
    @ words alph t n

let main s =
  let lines = s |> String.split_on_char '\n'
                |> List.map String.trim
                |> Array.of_list in
  let chars = lines.(0) |> String.split_on_char ' '
                        |> List.map String.trim in
  let n = Scanf.sscanf lines.(1) "%d" (fun x -> x) in
  List.iter print_endline (words chars chars n)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
