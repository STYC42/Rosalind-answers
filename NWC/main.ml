let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

exception NegCycle

let nwc n adj =
        let res = Array.make n 0 in
        for _ = 0 to n-1 do
                for i = 0 to n-1 do
                        List.iter (fun (ngh, w) ->
                                if res.(ngh) > res.(i)+w then
                                                res.(ngh) <- res.(i)+w
                        ) adj.(i)
                done
        done;
        try for i = 0 to n-1 do
                List.iter (fun (ngh, w) ->
                        if res.(ngh) > res.(i) + w then raise NegCycle
                ) adj.(i)
        done; false
        with NegCycle -> true


let main s =
        let ic = Scanf.Scanning.from_string s in
        let k = read_int ic in
        for _ = 0 to k-1 do
                let n = read_int ic in
                let m = read_int ic in
                let adj = Array.make n [] in
                for _ = 0 to m-1 do
                        let a = read_int ic in
                        let b = read_int ic in
                        let w = read_int ic in
                        adj.(a-1) <- (b-1, w) :: adj.(a-1)
                done;
                if nwc n adj then print_string "1 " else print_string "-1 "
        done

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
