let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let prob gc = function
        | 'A' | 'T' -> (1. -. gc) /. 2.
        | 'C' | 'G' -> gc /. 2.
        | _ -> failwith ""

 let prob_string s g =
  s |> String.to_seq
    |> Seq.map (prob g)
    |> Seq.fold_left ( *.) 1.

let expected_occurrences n s gc =
  let k = String.length s in
  let positions = float_of_int (n - k + 1) in
  if n < k then 0.0
  else positions *. prob_string s gc       

let main s =
        let n, s, a =
        s |> String.split_on_char '\n'
          |> List.map String.trim
          |> function a::b::c::_ ->
                (Scanf.sscanf a " %d" (fun i -> i),
                 b,
                 c |> String.split_on_char ' '
                   |> List.map String.trim
                   |> List.map (fun x -> Scanf.sscanf x " %f" (fun i -> i)))
             | _ -> failwith "Not implemented" in
        a |> List.map (expected_occurrences n s)
          |> List.iter (Printf.printf "%f ")


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
