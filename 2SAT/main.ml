let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let ind n a = if a > 0 then a + n - 1 else a + n
let opp n a = 2*n - 1 - a

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
        let adj_res = Array.make n [] in
        let app = Array.make n (-1) in
        let cnt = ref 0 in
        let rec dfs i = 
                app.(i) <- !cnt;
                List.iter (fun ngh -> 
                        if app.(ngh) = -1 then 
                                dfs ngh 
                        else 
                                adj_res.(!cnt) <- app.(ngh) :: adj_res.(!cnt)
                        ) adj_t.(i)
        in
        List.iter( fun i ->
                if app.(i) = -1 then begin
                        dfs i;
                        incr cnt
                end
        ) ts;
        !cnt, app, transp !cnt (Array.sub adj_res 0 !cnt)

exception False

let sat2 n adj =
        let res = Array.make (2*n) 0 in
        let k, app, cc = scc (2*n) adj in
        let lcc = Array.make k [] in
        for i = 0 to 2*n-1 do
                lcc.(app.(i)) <- i :: lcc.(app.(i))
        done;
        let ts = topo_sort k cc in
        try
                for i = 0 to n-1 do
                        if app.(i) = app.(opp n i) then
                                raise False
                done;
                List.iter (fun c ->
                        if res.(List.hd lcc.(c)) = 0 then begin
                                List.iter (fun x -> res.(x) <- 1) lcc.(c);
                                List.iter (fun x -> res.(opp n x) <- -1) lcc.(c)
                        end) (List.rev ts);
                Some (Array.init n (fun i -> res.(n+i)))
        with False -> None
                        

let main s =
  let ic = Scanf.Scanning.from_string s in
  let k = read_int ic in
  for _ = 0 to k-1 do
        let n = read_int ic in
        let m = read_int ic in
        let adj = Array.make (2*n) [] in
        for _ = 0 to m-1 do
                let a = read_int ic in
                let b = read_int ic in
                let i_a = ind n a in
                let i_b = ind n b in
                adj.(opp n i_a) <- i_b :: adj.(opp n i_a);
                adj.(opp n i_b) <- i_a :: adj.(opp n i_b)
        done;
        match sat2 n adj with
        | None -> print_endline "0"
        | Some a -> begin
                print_string "1 ";
                Array.iteri (fun i b -> Printf.printf "%d " (if b=1 then i+1 else -i-1)) a;
                print_newline () end
  done

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
