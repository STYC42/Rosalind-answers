let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s


let main s =
  let count = Array.make 4 0 in
  String.iter 
    (function
      | 'A' -> count.(0) <- count.(0) + 1
      | 'C' -> count.(1) <- count.(1) + 1
      | 'G' -> count.(2) <- count.(2) + 1
      | 'T' -> count.(3) <- count.(3) + 1
      | c -> Printf.printf "Symbol unexpected from a DNA string : %c\n" c;
             exit 1
    ) s;
    Printf.printf "%d %d %d %d\n" count.(0) count.(1) count.(2) count.(3)
  


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input