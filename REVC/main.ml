let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let reverse_string s =
  let len = String.length s in
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i s.[len - 1 - i]
  done;
  Bytes.to_string res
  

let main s =
  print_endline
    (reverse_string
      (String.map
        (function
          | 'A' -> 'T'
          | 'C' -> 'G'
          | 'G' -> 'C'
          | 'T' -> 'A'
          | c -> Printf.printf "Symbol unexpected from a DNA string : %c\n" c;
                 exit 1
        ) s
      )
    )

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
