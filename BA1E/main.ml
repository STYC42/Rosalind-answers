module StringSet = Set.Make(String)

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let rec decompos a =
    if a = 0 then [] else begin
    (a land ((1 lsl 31) - 1)) :: decompos (a lsr 31)
    end
  
let rec sum l =
  match l with
  | [] -> 0
  | h :: t -> h + sum t

let rec calcule_mod a =
  if a >= (1 lsl 31 - 1) then begin
    a |> decompos |> sum |> calcule_mod
  end else a

let hash_prog o p s e =
  let ret = ref s in
  for _ = 1 to p - 1 do
    ret := !ret lsl 8 |> calcule_mod
  done;
  let crt = (o - !ret) in
  if crt < 0 then begin
    (((crt + (1 lsl 31 - 1)) lsl 8) + e) |> calcule_mod
  end else
    ((crt lsl 8) + e) |> calcule_mod

let hash s =
  let res = ref 0 in
  Array.iter (fun e -> res := ((!res lsl 8) + e) |> calcule_mod) s;
  calcule_mod !res

let subs w l =
  let n, p = String.length l, String.length w in
  let res = ref [] in
  let txt = Array.init n (fun i -> int_of_char l.[i]) in
  let wrd = Array.init p (fun i -> int_of_char w.[i]) in
  let hash_wrd = hash wrd in
  let hash_sub = Array.sub txt 0 p |> hash |> ref in
  for i = p to pred n do
    assert (hash (Array.sub txt (i-p) p) = !hash_sub);
    if !hash_sub = hash_wrd then
      if String.sub l (i-p) p = w then
        res := (i-p) :: !res;
    hash_sub := hash_prog !hash_sub p txt.(i-p) txt.(i)
  done;
  if !hash_sub = hash_wrd then
    if String.sub l (n-p) p = w then
      res := (n-p) :: !res;
  !res

let has_clump strand l t mer =
  let k = String.length mer in
  let occ = subs mer strand in
  let window = Queue.create () in
  let max_length = ref 0 in
  List.iter (
    fun pos ->
      Queue.push pos window;
      while Queue.peek window < pos+k-l do
        Queue.drop window
      done;
      max_length := max !max_length (Queue.length window)
  ) occ;
  !max_length >= t

let mers k strand =
  let res = ref StringSet.empty in
  for i = 0 to String.length strand - k do
    res := StringSet.add (String.sub strand i k) !res
  done;
  StringSet.to_list !res

let main s =
  let strand, k, l, t = Scanf.sscanf s "%s\n%d %d %d" (fun a b c d -> a, b, c, d) in
  mers k strand |> List.filter (has_clump strand l t)
                |> List.iter (Printf.printf "%s ");
  print_newline ()

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
