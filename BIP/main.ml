let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

exception NotBip

let bip n adj =
        let clr = Array.make n 2 in
        let rec dfs i =
                List.iter (fun ngh ->
                        if clr.(ngh) = clr.(i) then 
                                raise NotBip
                        else if clr.(ngh) = 2 then begin
                                clr.(ngh) <- 1 - clr.(i);
                                dfs ngh
                        end) adj.(i)
        in
        try 
                for i = 0 to n-1 do
                        if clr.(i) = 2 then begin
                                clr.(i) <- 0;
                                dfs i
                        end
                done; 1
        with NotBip -> -1


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
                  adj.(a-1) <- b-1 :: adj.(a-1);
                  adj.(b-1) <- a-1 :: adj.(b-1)
          done;
          Printf.printf "%d " (bip n adj)
  done


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
