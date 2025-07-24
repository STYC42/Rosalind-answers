module SplitSet = Set.Make(String)
module StringSet = Set.Make(String)
module IntSet = Set.Make(Int)

type tree = Node of (tree list) | Leaf of string

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

  let (%+) a b =
  (((a+b) mod 1000000) + 1000000) mod 1000000

let (%*) a b =
  (((a*b) mod 1000000) + 1000000) mod 1000000

let rec sum_list l =
  match l with
  | [] -> 0
  | h :: t -> h %+ sum_list t

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

let distance root1 root2 =
  let nds1 = nodes root1 in
  let nds2 = nodes root2 in
  let set1 = nds1 |> List.tl
                  |> List.map (split_tree root1)
                  |> SplitSet.of_list in
  let set2 = nds2 |> List.tl
                  |> List.map (split_tree root2)
                  |> SplitSet.of_list in
  let inter = SplitSet.inter set1 set2 in
  let dista = SplitSet.diff set1 set2 in
  let distb = SplitSet.diff set2 set1 in
  inter, dista, distb

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
  let dyn = Hashtbl.create 1000000 in
  let rec aux a b i l =
    if IntSet.cardinal a < 2 || IntSet.cardinal b < 2 then 0 else
    match Hashtbl.find_opt dyn (a, b, i, l) with
    | Some a -> 
        (Printf.fprintf stderr "\rLe dictionnaire a été utile ! (%d)\n" (List.length l); a)
    | None -> (let res = begin
                match l with
                | [] -> (
                  Printf.fprintf stderr "\rqrt_brut (%d), hashtbl (%d)" i (Hashtbl.length dyn); 
                  flush stderr;
                  if i mod 2 = 1 then
                    qrt_brut a b
                  else
                    (- qrt_brut a b))
                | (c, d) :: t ->
                  aux a b i t +
                  aux (IntSet.inter a c) (IntSet.inter b d) (succ i) t
                end in
              Hashtbl.add dyn (a, b, i, l) res;
              res)
  in
  match l with
  | [] -> 0
  | (a, b) :: t -> 
    (Printf.fprintf stderr "\rpoincar: %d\n" (List.length t); flush stderr;
    let res = aux a b 1 t in
    res + poincar t)



let qrt splits =
  splits
  |> SplitSet.to_list
  |> List.map String.trim
  |> List.map split
  |> poincar

let diff_character c1 c2 =
  let z = ref 0 in
  let o = ref 0 in
  let res = String.mapi (
    fun i c ->
      if c = '0' && c2.[i] = '0' then begin
        incr z;
        '0' 
      end else if c = '1' && c2.[i] = '1' then begin
        incr o;
        '1'
      end else 'x'
  ) c1 in
  if !z < 2 || !o < 2 then
    None
  else Some res

exception Abort

let inclus c1 c2 =
  try
    for i = 0 to String.length c1 - 1 do
      if c1.[i] <> c2.[i] && c1.[i] <> 'x' then raise Abort
    done;
    true
  with Abort -> false

let () = assert (diff_character "10100" "10001" = None)

let simp_naive l =
  let rec aux g acc =
    match g with
    | [] -> acc
    | h :: t ->
      aux t (inseren h acc)
  and inseren e acc =
    match acc with
    | [] -> [e]
    | h :: _t when inclus e h -> acc
    | h :: t when inclus h e -> e :: t
    | h :: t -> h :: inseren e t
  in aux l []

let simp_fusion l1 l2 =
  let cmp = ref l1 in
  let res = ref [] in
  let rec inseref e c =
    match c with
    | [] -> res := e :: !res; []
    | h :: t when inclus e h -> h :: t
    | h :: t when inclus h e -> res := e :: !res; t
    | h :: t -> h :: inseref e t
  in
  let rec aux k =
    match k with
    | [] -> ()
    | h :: t ->
      cmp := inseref h !cmp; aux t
  in
  aux l2;
  !res @ !cmp
  

let rec simp l =
  if List.length l < 1000 then simp_naive l else begin
  let l1 = List.take (List.length l /2) l in
  let l2 = List.drop (List.length l /2) l in
  let s1, s2 = simp l1, simp l2 in
  Printf.fprintf stderr "\rfusion %d (%d)" (List.length l) (List.length s1 + List.length s2); flush stderr;
  simp_fusion s1 s2 end

let diff_characters s1 s2 =
  let j = ref 0 in
  let n = ref 0 in
  StringSet.fold (
    fun x acc1 ->
      let y = StringSet.fold (
                fun w acc2 ->
                  if !n mod 100000 = 0 then
                  Printf.fprintf stderr "\rdiff %d %d" (List.length acc1) !n; flush stderr;
                  incr n;
                  let dc = diff_character x w in
                  match dc with
                  | None -> acc2
                  | Some a -> incr j;
                    a :: acc2
              ) s2 [] in
      y @ acc1
  ) s1 []

let expend_quadra a b =
  let d = diff_characters a b in
  let s = simp d in
  StringSet.of_list s

let main s =
  let lines = String.split_on_char '\n' s in
  let root1 = build_tree (List.nth lines 1) in
  let root2 = build_tree (List.nth lines 2) in
  let common, dista, distb = distance root1 root2 in
  let cq = qrt common in
  let dc = expend_quadra dista distb in
  StringSet.iter prerr_endline dc;
  let res = qrt dc in
  Printf.printf "%d\n" 
    (nb_quartets root1 + 
     nb_quartets root2 - 
     (2*(cq + res)))


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input