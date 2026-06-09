let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

exception False

let dag n adj =
        let vst = Array.make n 0 in
        let rec dfs i =
                vst.(i) <- 1;
                List.iter (fun ngh -> 
                        if vst.(ngh) = 1 then raise False
                        else dfs ngh) adj.(i);
                vst.(i) <- 2
        in
        try
                for i = 0 to n-1 do
                        if vst.(i) = 0 then dfs i
                done;
                1
        with False -> -1

let main s =
        let ic = Scanf.Scanning.from_string s in
        let k = read_int ic in
        for _ = 0 to k-1 do
                let n = read_int ic in
                let m = read_int ic in
                let adj = Array.make n [] in
                for _ = 0 to m-1 do
                        let a = read_int ic -1 in
                        let b = read_int ic -1 in
                        adj.(a) <- b :: adj.(a)
                done;
                Printf.printf "%d " (dag n adj)
        done


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
