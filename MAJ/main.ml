let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let maj a n =
        let c = Hashtbl.create 1 in
        Array.iter (fun x -> Hashtbl.add c x 0) a;
        match begin 
                Hashtbl.to_seq_keys c
                |> List.of_seq
                |> List.map (fun x -> (x, x |> Hashtbl.find_all c |> List.length))
                |> List.filter (fun (_, y) -> y >= n/2+1)
                |> List.map fst
        end with
        | [] -> -1
        | h :: _ -> h

let main s =
        let ic = Scanf.Scanning.from_string s in
        let k = read_int ic in
        let n = read_int ic in
        for _ = 0 to k-1 do
                let a = Array.init n (fun _ -> read_int ic) in
                Printf.printf "%d " (maj a n)
        done;
        print_newline ()
  


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
