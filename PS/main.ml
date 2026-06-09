let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let child_l j = 2*j+1
let child_r j = 2*j+2

let rec bubble n a j =
        match a.(j),
                (if child_l j < n then a.(child_l j) else (a.(j)+1)),
                (if child_r j < n then a.(child_r j) else (a.(j)+1)) with
                | p, l, r when l < p && l <= r ->
                        a.(j) <- l; a.(child_l j) <- p;
                        bubble n a (child_l j)
                | p, l, r when r < p && r <= l ->
                        a.(j) <- r; a.(child_r j) <- p;
                        bubble n a (child_r j)
                | _ -> ()


let heapify n a =
        for i = n-1 downto 0 do
                bubble n a i
        done

let part_sort n a k =
        let res = Array.make k 0 in
        heapify n a;
        for i = 0 to k-1 do
                res.(i) <- a.(0);
                a.(0) <- max_int-1;
                bubble n a 0
        done;
        res

let main s =
        let ic = Scanf.Scanning.from_string s in
        let n = read_int ic in
        let a = Array.init n (fun _ -> read_int ic) in
        let k = read_int ic in
        let res = part_sort n a k in
        Array.iter (Printf.printf "%d ") res


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
