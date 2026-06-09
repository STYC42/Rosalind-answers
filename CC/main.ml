let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let nb_cc n adj =
        let vst = Array.make n false in
        let cnt = ref 0 in
        let rec dfs i =
                vst.(i) <- true;
                List.iter 
                        (fun ngh -> if not vst.(ngh) then dfs ngh) 
                        adj.(i)
        in
        for i = 0 to n-1 do
                if not vst.(i) then begin
                        incr cnt;
                        dfs i
                end
        done;
        !cnt

let main s =
        let ic = Scanf.Scanning.from_string s in
        let n = read_int ic in
        let m = read_int ic in
        let adj = Array.make n [] in
        for _ = 0 to m-1 do
                let a = read_int ic in
                let b = read_int ic in
                adj.(a-1) <- b-1 :: adj.(a-1);
                adj.(b-1) <- a-1 :: adj.(b-1)
        done;
        Printf.printf "%d\n" (nb_cc n adj)



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
