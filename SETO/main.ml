module IntSet = Set.Make(Int)

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s


let print_set s =
  let l = IntSet.to_list s in
  print_string "{";
  l |> List.map string_of_int
    |> String.concat ", "
    |> print_string;
  print_endline "}"


let main s =
  let lines = String.split_on_char '\n' s in
  let n = Scanf.sscanf (List.nth lines 0) "%d" (fun a -> a) in
  let a = Scanf.sscanf (List.nth lines 1) "{%[^}]}" (fun s -> s)
          |> String.split_on_char ','
          |> List.map String.trim
          |> List.map int_of_string
          |> IntSet.of_list in
  let b = Scanf.sscanf (List.nth lines 2) "{%[^}]}" (fun s -> s)
          |> String.split_on_char ','
          |> List.map String.trim
          |> List.map int_of_string
          |> IntSet.of_list in
  let e = IntSet.of_list (List.init n (fun i -> i+1)) in
  print_set (IntSet.union a b);
  print_set (IntSet.inter a b);
  print_set (IntSet.diff a b);
  print_set (IntSet.diff b a);
  print_set (IntSet.diff e a);
  print_set (IntSet.diff e b)
  


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
