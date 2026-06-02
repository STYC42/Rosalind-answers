let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let part a p r =
        let x = a.(r) in
        let i = ref (p-1) in
        for j = p to r-1 do
                if a.(j) <= x then begin
                        incr i;
                        let t = a.(j) in
                        a.(j) <- a.(!i);
                        a.(!i) <- t
                end
        done;
        let t = a.(r) in
        a.(r) <- a.(!i+1);
        a.(!i+1) <- t;
        !i+1

let part_r a p r =
        let i = Random.int_in_range ~min:p ~max:r in
        let t = a.(r) in
        a.(r) <- a.(i);
        a.(i) <- t;
        part a p r

let sort a =
        let rec aux p r =
                if r-p >= 1 then begin
                        let i = part_r a p r in
                        aux p (i-1);
                        aux (i+1) r
                end
        in aux 0 (Array.length a - 1)


let main s =
        Random.self_init ();
        let ic = Scanf.Scanning.from_string s in
        let n = read_int ic in
        let a = Array.init n (fun _ -> read_int ic) in
        sort a;
        Array.iter (Printf.printf "%d ") a


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
