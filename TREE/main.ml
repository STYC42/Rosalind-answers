let main filename =
  let ic = Scanf.Scanning.open_in filename in
  let n = Scanf.bscanf ic "%d" (fun a -> a) in
  let con = ref [] in
  try
    while true do
      let a, b = Scanf.bscanf ic " %d %d" (fun a b -> (a, b)) in
      con := (a, b) :: !con
    done
  with End_of_file -> ();
  Printf.printf "%d\n" (n - List.length !con - 1)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  main Sys.argv.(1)
