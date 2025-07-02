module StringSet = Set.Make(String)
module CoupleSet = Set.Make

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let set_to_string s =
  "{"^(s |> StringSet.to_seq
         |> List.of_seq
         |> String.concat ", ")^"} "

let split e descr =
  let a = ref StringSet.empty in
  let b = ref StringSet.empty in
  String.iteri (
    fun i c ->
      match c with
      | '0' -> b := StringSet.add e.(i) !b
      | '1' -> a := StringSet.add e.(i) !a
      | _ -> ()
  ) descr;
  !a, !b

let parts2 s =
  let res = ref [] in
  let n = StringSet.empty in
  StringSet.iter (
    fun l ->
      let m = StringSet.add l n in
      StringSet.iter (
        fun k ->
          let o = StringSet.add k m in
          if StringSet.cardinal o = 2 then
            res := o :: !res
      ) s
  ) s;
  !res

let infer h (a, b) =
  let pa = parts2 a in
  let pb = parts2 b in
  List.iter (
    fun e ->
      List.iter (
        fun f ->
          let se = set_to_string e in
          let sf = set_to_string f in
          match Hashtbl.find_opt h (se^sf), 
                Hashtbl.find_opt h (sf^se) with
          | None, None ->
            Hashtbl.replace h (se^sf) 0
          | Some _, _ -> ()
          | _, Some _ -> ()

      ) pb
  ) pa

let main s =
  let lines = String.split_on_char '\n' s in
  let h = Hashtbl.create 1 in
  let el = List.hd lines
           |> String.trim
           |> String.split_on_char ' '
           |> List.map String.trim
           |> Array.of_list in
  List.tl lines
  |> List.map String.trim
  |> List.map (split el) 
  |> List.iter (infer h);
  Hashtbl.iter (
    fun k _v ->
      print_endline k
  ) h
  


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
