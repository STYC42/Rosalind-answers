let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let rec fact_ = function
        | 0 -> 1
        | n -> n * fact_ (pred n)

let fact = 
        let d = Array.init 11 fact_ in
        fun i -> assert (i < 11); d.(i)

let hash a =
        let rec aux = function
                | [] -> 0
                | h :: t -> h * fact (List.length t)
                        + aux (List.map (fun x -> if x > h then x-1 else x) t)
        in aux (Array.to_list a)

let () =
        assert (hash [| 0; 1; 2; 3 |] = 0);
        assert (hash [| 3; 2; 1; 0 |] = 23);
        assert (hash [| 2; 1; 0; 3 |] = 14)

let swap a i j =
        assert (0 <= i && i < 10 && 0 <= j && j < 10);
        let t = a.(j) in
        a.(j) <- a.(i);
        a.(i) <- t

let rev a i j =
        assert (i < j-1);
        for k = 0 to (j-i)/2-1 do
                swap a (i+k) (j-1-k)
        done

let () = 
        let a = [| 0; 1; 2; 3; 4; 5 |] in
        rev a 1 3;
        assert (a = [| 0; 2; 1; 3; 4; 5 |])

let dyn = let r = Array.make (fact 10) None in r.(0) <- Some (0, (0, 0)); r

let rec dist a =
        let id = hash a in
        match dyn.(id) with
        | None ->
                let m = ref 10 in
                let mn = ref (0, 0) in
                for i = 0 to 9 do for j = i+2 to 10 do
                        rev a i j;
                        let id' = hash a in
                        if id' < id then begin
                                let r = dist a in
                                if !m > r then begin
                                        m := r;
                                        mn := (i, j)
                        end end;
                        rev a i j
                done done;
                dyn.(id) <- Some (!m + 1, !mn);
                (!m + 1)
        | Some (t, _) -> t

let rec path a =
        if hash a = 0 then []
        else match dyn.(hash a) with
        | None -> failwith "Not Implemented"
        | Some (_, (i, j)) ->
                        rev a i j;
                        (i, j) :: path a
                        

let main s =
  let ic = Scanf.Scanning.from_string s in
          let a = Array.make 10 0 in
          for i = 0 to 9 do
                  let x = read_int ic in
                  a.(x-1) <- i
          done;
          let b = Array.init 10 (fun _ -> a.(read_int ic - 1)) in
          Printf.printf "%d\n" (dist b);
          let p = path b in
          List.iter (fun (i, j) -> Printf.printf "%d %d\n" (i+1) j) (List.rev p)


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
