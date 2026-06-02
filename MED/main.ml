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

let rec select_r a p r i =
        if p = r then a.(p)
        else begin
                let q = part_r a p r in
                let k = q-p+1 in
                if i = k then a.(q)
                else if i < k then select_r a p (q-1) i
                else select_r a (q+1) r (i-k)
        end

let main s =
        Random.self_init ();
        let ic = Scanf.Scanning.from_string s in
        let n = read_int ic in
        let a = Array.init n (fun _ -> read_int ic) in
        let k = read_int ic in
        Printf.printf "%d\n" (select_r a 0 (n-1) k)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
