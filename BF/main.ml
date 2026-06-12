let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let bf n adj =
        let res = Array.make n None in
        res.(0) <- Some 0;
        for _ = 0 to n-1 do
                for i = 0 to n-1 do
                        match res.(i) with
                        | None -> ()
                        | Some d ->
                                List.iter (fun (ngh, w) ->
                                        match res.(ngh) with
                                        | None -> res.(ngh) <- Some (d+w)
                                        | Some d' -> if d' > d+w then
                                                res.(ngh) <- Some (d+w)
                                ) adj.(i)
                done
        done;
        res


let main s =
        let ic = Scanf.Scanning.from_string s in
        let n = read_int ic in
        let m = read_int ic in
        let adj = Array.make n [] in
        for _ = 0 to m-1 do
                let a = read_int ic in
                let b = read_int ic in
                let w = read_int ic in
                adj.(a-1) <- (b-1, w) :: adj.(a-1)
        done;
        Array.iter (function None -> print_string "x " | Some i -> Printf.printf "%d " i) (bf n adj)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
