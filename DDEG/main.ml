let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let main s =
        let ic = Scanf.Scanning.from_string s in
        let n = Scanf.bscanf ic "%d" (fun j->j) in
        let m = Scanf.bscanf ic " %d" (fun j->j) in
        let adj = Array.make n [] in
        for _ = 1 to m do
                let a, b = Scanf.bscanf ic " %d %d" (fun j k->(j, k)) in
                adj.(a-1) <- (b-1) :: adj.(a-1);
                adj.(b-1) <- (a-1) :: adj.(b-1)
        done;
        Array.iter (fun x ->
                Printf.printf "%d "
                (List.fold_left (+) 0
                (List.map (fun i -> List.length adj.(i)) x))
        ) adj;
        print_newline ()


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
