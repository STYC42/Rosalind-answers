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

let main s =
  let n = Scanf.sscanf s "%d" (fun a -> a) in
  let r = List.init n (fun i -> succ i) in
  let p = perm r in
  Printf.printf "%d\n" (List.length p);
  List.iter
  (fun l -> 
    List.iter (Printf.printf "%d ") l; 
    print_newline ()
  ) p 


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
