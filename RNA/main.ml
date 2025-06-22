let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s


let main s =
  Printf.printf "%s\n" 
  (String.map
    (function
      | 'A' -> 'A'
      | 'C' -> 'C'
      | 'G' -> 'G'
      | 'T' -> 'U'
      | c -> Printf.printf "Symbol unexpected from a DNA string : %c\n" c;
            exit 1
      ) s
  )


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
