let longest_chain_topo g comp =
  (* En fonction du tri topologique g et de la comparaison comp qui indique une liaison,
     renvoie le plus long chemin dans le DAG *)
  let dyn = Array.make (Array.length g) 0 in
  let prd = Array.make (Array.length g) (-1) in
  for i = 0 to pred (Array.length g) do
    let m = ref 0 in
    for j = 0 to pred i do
      if comp g.(i) g.(j) && !m < dyn.(j) then begin
        m := dyn.(j);
        prd.(i) <- j
      end
    done;
    dyn.(i) <- !m + 1
  done;
  let res = ref [] in
  let mx = Array.fold_left max 0 dyn in
  let cur = ref (match Array.find_index (fun x -> x = mx) dyn with None -> -1 | Some a -> a) in
  while !cur <> -1 do
    res := g.(!cur) :: !res;
    cur := prd.(!cur)
  done;
  !res
  
let lgis p =
  List.iter (Printf.printf "%d ") (longest_chain_topo p (>));
  print_newline ();
  List.iter (Printf.printf "%d ") (longest_chain_topo p (<));
  print_newline ()

let main () =
  let input = Sys.argv.(1) in
  let file = Scanf.Scanning.open_in input in
  let read_int () = Scanf.bscanf file " %d" (fun x -> x) in
  let n = read_int () in
  let p = Array.init n (fun _ -> read_int ()) in
  lgis p


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  main ()
