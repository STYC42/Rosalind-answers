let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let main s =
  let skew = ref 0 in
  let vmin = ref max_int in
  let lmin = ref [] in
  String.iteri (
    fun i ->
      function
      | 'A' -> ()
      | 'C' -> decr skew;
              if !skew < !vmin then begin
                vmin := !skew;
                lmin := [i+1]
              end else if !skew = !vmin then
                lmin := (i+1) :: !lmin
      | 'G' -> incr skew
      | 'T' -> ()
      | _ -> failwith "character unexpected from a DNA strand"
  ) s;
  List.iter (Printf.printf "%d ") (List.rev !lmin);
  print_newline ()


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
