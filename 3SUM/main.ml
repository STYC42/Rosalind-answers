let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

exception Found of int*int*int

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let sum3 a =
        let h = Hashtbl.create 1 in
        Array.iteri (fun i x ->
                Array.iteri (fun j y ->
                        if i < j then
                                Hashtbl.add h (-x-y) (i, j)
                ) a
        ) a;
        try
                Array.iteri (fun l w ->
                        let fa = Hashtbl.find_all h w in
                        List.iter (fun (i, j) ->
                                if l <> i && l <> j then
                                        raise (Found (i, j, l))
                        ) fa
                ) a;
                None
        with Found (i, j, k) -> Some (i, j, k)

let main s =
  let ic = Scanf.Scanning.from_string s in
  let k = read_int ic in
  let n = read_int ic in
  for _ = 0 to k-1 do
          let a = Array.init n (fun _ -> read_int ic) in
          match sum3 a with
          | None -> print_endline "-1"
          | Some (i, j, k) -> Printf.printf "%d %d %d\n" i j k
  done


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
