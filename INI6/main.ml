let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let main s =
  let words = s |> String.split_on_char ' '
                |> List.map String.trim in
  let dict = Hashtbl.create 1 in
  List.iter (
    fun w ->
      match Hashtbl.find_opt dict w with
      | None -> Hashtbl.add dict w 1
      | Some n -> Hashtbl.replace dict w (n+1)
  ) words;
  Hashtbl.iter (Printf.printf "%s %d\n") dict


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
