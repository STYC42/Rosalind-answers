let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let topo_sort n adj =
        let vst = Array.make n false in
        let res = ref [] in
        let rec dfs i =
                vst.(i) <- true;
                List.iter (fun ngh ->
                        if not vst.(ngh) then dfs ngh
                ) adj.(i);
                res := i :: !res
        in
        for i = 0 to n-1 do
                if not vst.(i) then dfs i
        done;
        !res

let transp n adj =
        let res = Array.make n [] in
        Array.iteri (fun i -> List.iter (fun j -> res.(j) <- i :: res.(j))) adj;
        res

let scc n adj =
        let ts = topo_sort n adj in
        let adj_t = transp n adj in
        let vst = Array.make n false in
        let rec dfs i = 
                vst.(i) <- true;
                List.iter (fun ngh -> if not vst.(ngh) then dfs ngh) adj_t.(i)
        in
        let cnt = ref 0 in
        List.iter( fun i ->
                if not vst.(i) then begin
                        incr cnt;
                        dfs i
                end
        ) ts;
        !cnt
                        

let main s =
  let ic = Scanf.Scanning.from_string s in
  let n = read_int ic in
  let m = read_int ic in
  let adj = Array.make n [] in
  for _ = 0 to m-1 do
          let a = read_int ic - 1 in
          let b = read_int ic - 1 in
          adj.(a) <- b :: adj.(a)
  done;
  Printf.printf "%d\n" (scc n adj)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
