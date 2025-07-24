module IntSet = Set.Make(Int)

type tree = Node of (tree list) | Leaf of string

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let build_tree s =
  let ic = Scanf.Scanning.from_string s in
  let rec aux acc =
    match Scanf.bscanf ic "%c" (fun a -> a) with
    |';' -> 
      assert (List.length acc = 1);
      Some (List.hd acc)
    | ')' -> begin
      match acc with
      | [] -> None
      | _h :: _t -> Some (Node acc) end
    | '(' -> begin
      match aux [] with
      | None -> aux acc
      | Some child -> aux (child :: acc) end
    | ',' -> aux acc
    | e -> 
      let o = Scanf.bscanf ic "%[^,)]" (fun a -> a) in
      aux (Leaf ((String.make 1 e)^o) :: acc)
  in
  match aux [] with
  | None -> failwith "Tree is empty"
  | Some a -> a

let nb_couples n =
  (n * (n-1)) / 2

let rec leaves node =
  match node with
  | Leaf a -> [a]
  | Node l -> l |> List.map leaves
                |> List.flatten

let rec nodes node =
  match node with
  | Leaf _ -> []
  | Node l -> node :: (l |> List.map nodes
                         |> List.flatten )
  
let split_tree root node =
  assert (root != node);
  let pos = Hashtbl.create 1 in
  root |> leaves
      |> List.sort compare
      |> List.iteri (fun i lv -> Hashtbl.add pos lv i);
  let sep = leaves node in
  let res = Array.make (List.length (leaves root)) 0 in
  List.iter (
    fun c ->
      res.(Hashtbl.find pos c) <- 1
  ) sep;
  if res.(0) = 0 then Array.iteri (fun i x -> res.(i) <- 1-x) res;
  res |> Array.to_list
      |> List.map string_of_int
      |> String.concat ""

let ctbl tr =
  let nds = List.tl (nodes tr) in
  List.map (split_tree tr) nds

let split descr =
  let a = ref IntSet.empty in
  let b = ref IntSet.empty in
  String.iteri (
    fun i c ->
      match c with
      | '0' -> b := IntSet.add i !b
      | '1' -> a := IntSet.add i !a
      | _ -> ()
  ) descr;
  !a, !b

let qrt_brut a b =
  let n = IntSet.cardinal a in
  let k = IntSet.cardinal b in
  let res = nb_couples n * nb_couples k in
  res

let rec poincar l =
  let rec aux a b i l =
    if IntSet.cardinal a < 2 || IntSet.cardinal b < 2 then begin
      0 
    end else begin
    match l with
    | [] -> (
      let res = 
        if i mod 2 = 1 then
          qrt_brut a b
        else
          (- qrt_brut a b) in
        res)
    | (c, d) :: t ->(
      if (List.length l > 3600) then begin
        prerr_char '-'; flush stderr;
        let drop = aux a b i t in
        prerr_string "\b+";flush stderr;
        let take = aux (IntSet.inter a c) (IntSet.inter b d) (succ i) t in
        prerr_char '\b'; flush stderr;
        take+drop
      end else
        aux a b i t + aux (IntSet.inter a c) (IntSet.inter b d) (succ i) t
      )
    end
  in
  match l with
  | [] -> 0
  | (a, b) :: t -> 
    prerr_newline ();
    aux a b 1 t  + poincar t

let qrt splits =
  splits
  |> List.map String.trim
  |> List.map split
  |> poincar


let (%+) a b =
  (((a+b) mod 1000000) + 1000000) mod 1000000

let (%*) a b =
  (((a*b) mod 1000000) + 1000000) mod 1000000

let rec sum_list l =
  match l with
  | [] -> 0
  | h :: t -> h %+ sum_list t

let rec leaves node =
  match node with
  | Leaf a -> [a]
  | Node l -> l |> List.map leaves
                |> List.flatten

let nb_couples n =
  (n * (n-1)) / 2

let nb_quartets tr =
  let n_tot = tr |> leaves |> List.length in
  let rec aux par node =
    let n_int = node |> leaves |> List.length in
    let n_ext = (par |> leaves |> List.length) - n_int in
    match node with
    | Leaf _ -> 0
    | Node l ->
      let lower = l |> List.map (aux node) |> sum_list in
      let lower_couples = l |> List.map leaves 
                            |> List.map List.length 
                            |> List.map nb_couples in
      let sum_ncouples = sum_list lower_couples in
      let too_much = lower_couples |> List.map 
                                        (fun x -> x %* (sum_ncouples %+ (-x)))
                                   |> sum_list in
      let insid_gross = nb_couples n_ext %* nb_couples n_int in
      let insid = insid_gross %+ (-too_much/2) in
      let outsid = n_ext %* (n_tot - n_ext - n_int) %* nb_couples n_int in
      lower %+ insid %+ outsid
  in
  match tr with
  | Leaf _ -> 0
  | Node l ->
    let lower_couples = l |> List.map leaves 
                            |> List.map List.length 
                            |> List.map nb_couples in
    let sum_ncouples = sum_list lower_couples in
    let too_much = lower_couples |> List.map 
                                      (fun x -> x %* (sum_ncouples %+ (-x)))
                                  |> sum_list in
    List.fold_left (%+) 0 (List.map (aux tr) l) %+ (-too_much/2)
  

let main s =
  let lines = String.split_on_char '\n' s in
  let root1 = build_tree (List.nth lines 1) in
  let root2 = build_tree (List.nth lines 2) in
  let ctbl1 = ctbl root1 in
  let ctbl2 = ctbl root2 in
  Printf.printf "%d\n" 
  (2 * qrt (ctbl1 @ ctbl2) 
  - nb_quartets root1 - nb_quartets root2)

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input