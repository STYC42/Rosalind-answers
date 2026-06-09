let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

exception True

let has_sq n adj = try
        for i = 0 to n-1 do for j = i+1 to n-1 do
                if adj.(i).(j) = 1 then begin
                        for s = 1 to (n-j-1) do for l = 1 to (n-i-1) do
                                if adj.(i+l).(j) = 1 && 
                                   adj.(i+l).(j+s) = 1 &&
                                   adj.(i).(j+s) = 1 then raise True
                        done done
                end
        done done; -1
        with True -> 1

let main s =
        let ic = Scanf.Scanning.from_string s in
        let k = read_int ic in
        for _ = 0 to k-1 do
                let n = read_int ic in
                let m = read_int ic in
                let adj = Array.make_matrix n n 0 in
                for _ = 0 to m-1 do
                        let a = read_int ic - 1 in
                        let b = read_int ic - 1 in
                        adj.(a).(b) <- 1;
                        adj.(b).(a) <- 1
                done;
                Printf.printf "%d " (has_sq n adj)
        done


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
