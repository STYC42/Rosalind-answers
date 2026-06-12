let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let bfs n adj =
        let res = Array.make n (-1) in
        let q = Queue.create () in
        Queue.push 0 q;
        res.(0) <- 0;
        while not (Queue.is_empty q) do
                let a = Queue.pop q in
                List.iter (fun b ->
                        if res.(b) = -1 then begin
                                res.(b) <- res.(a) + 1;
                                Queue.push b q
                        end
                ) adj.(a)
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
          adj.(a-1) <- b-1 :: adj.(a-1)
  done;
  Array.iter (Printf.printf "%d ") (bfs n adj)
          


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
