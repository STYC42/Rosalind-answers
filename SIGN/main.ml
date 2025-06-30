let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let rec insert x l =
  match l with
  | [] -> [[x]]
  | h :: t -> 
    (x :: l) :: (List.map (fun e -> h :: e) (insert x t))

let rec perm l =
  match l with
  | [] -> [l]
  | h :: t -> 
    List.flatten (List.map (insert h) (perm t))


let rec bits l n acc =
  if l = 0 then acc else
  bits (pred l) (n lsr 1) ((n land 1 = 1) :: acc)

let main s =
  let n = Scanf.sscanf s "%d" (fun a -> a) in
  let p = perm (List.init n (fun i -> i+1)) in
  Printf.printf "%d\n" ((List.length p) lsl n);
  List.iter (
    fun ord ->
      for i = 0 to pred (1 lsl n) do
        List.iter (
          Printf.printf "%d "
        ) (List.map2 (
             fun a b -> if b then a else -a
           ) ord (bits n i []));
        print_newline ()
      done
  ) p



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
