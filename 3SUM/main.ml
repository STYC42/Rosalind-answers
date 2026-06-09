let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

exception Found of int*int*int

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let sum3 a =
        let h = Hashtbl.create 100000 in
        for i = 0 to Array.length a - 1 do
                for j = i+1 to Array.length a - 1 do
                        Hashtbl.add h (-a.(i)-a.(j)) (i, j)
                done
        done;
        try
                for l = 0 to Array.length a - 1 do
                        let fa = Hashtbl.find_all h a.(l) in
                        List.iter (fun (i, j) ->
                                if l <> i && l <> j then
                                        raise (Found (i, j, l))
                        ) fa
                done;
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
          | Some (i, j, k) -> begin
                  [i+1; j+1; k+1] |> List.sort compare |> List.iter (Printf.printf "%d ");
                  print_newline ()
          end
  done


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
