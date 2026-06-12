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
                List.iter (fun (ngh, _) ->
                        if not vst.(ngh) then dfs ngh
                ) adj.(i);
                res := i :: !res
        in
        for i = 0 to n-1 do
                if not vst.(i) then dfs i
        done;
        !res

let sdag n adj =
        let ts = topo_sort n adj in
        let res = Array.make n None in
        res.(0) <- Some 0;
        List.iter (fun x ->
                match res.(x) with
                | None -> ()
                | Some d ->
                        List.iter (fun (ngh, w) -> 
                                match res.(ngh) with
                                | None -> res.(ngh) <- Some (d+w)
                                | Some d' -> res.(ngh) <- Some (min d' (d+w))
                        ) adj.(x)
        ) ts;
        res

let main s =
  let ic = Scanf.Scanning.from_string s in
  let n = read_int ic in
  let m = read_int ic in
  let adj = Array.make n [] in
  for _ = 0 to m-1 do
          let a = read_int ic - 1 in
          let b = read_int ic - 1 in
          let w = read_int ic in
          adj.(a) <- (b, w) :: (List.filter (fun (i, _) -> i <> a) adj.(a))
  done;
  Array.iter (function None -> print_string "x " | Some i -> Printf.printf "%d " i) (sdag n adj)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
