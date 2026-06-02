let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let inv a =
        let i = ref 0 in
        for k = 0 to Array.length a -1 do
                for l = k+1 to Array.length a -1 do
                        if a.(k) > a.(l) then incr i
                done
        done;
        !i

let main s =
  let ic = Scanf.Scanning.from_string s in
  let n = read_int ic in
  let a = Array.init n (fun _ -> read_int ic) in
  Printf.printf "%d" (inv a)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
