let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let fib (n: int): int =
        let rec aux (a: int) (b: int) (i: int): int =
                if i = 0 then a
                else if i = 1 then b
                else aux b (a+b) (i-1)
        in aux 0 1 n


let main s =
        let n = Scanf.sscanf s "%d" (fun i -> i) in
        Printf.printf "%d\n" (fib n)
  

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
