let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let main s =
  let ic = Scanf.Scanning.from_string s in
  let n = Scanf.bscanf ic "%d" (fun i-> i) in
  let a = Array.init n (fun _ -> Scanf.bscanf ic " %d"(fun j -> j)) in
  let k = ref 0 in
  for i = 1 to n-1 do
          let j = ref i in
          while !j > 0 && a.(!j-1) > a.(!j) do
                  let tmp = a.(!j-1) in
                  a.(!j-1) <- a.(!j);
                  a.(!j) <- tmp;
                  decr j;
                  incr k
          done
  done;
  Printf.printf "%d\n" !k


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
