let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let read_int ic = Scanf.bscanf ic " %d" (fun i -> i)

let child_l j = 2*j+1
let child_r j = 2*j+2
let parent j = (j-1)/2

let rec sift_down k a h j =
        match a.(j),
                a.(if child_l j < !k then child_l j else j),
                a.(if child_r j < !k then child_r j else j) with
                | (x, p), (xcl, pcl), (_, pcr) when pcl < p && pcl <= pcr ->
                        a.(j) <- (xcl, pcl); 
                        a.(child_l j) <- (x, p);
                        Hashtbl.replace h x (child_l j);
                        Hashtbl.replace h xcl j;
                        sift_down k a h (child_l j)
                | (x, p), (_, pcl), (xcr, pcr) when pcr < p && pcr <= pcl -> 
                        a.(j) <- (xcr, pcr); 
                        a.(child_r j) <- (x, p);
                        Hashtbl.replace h x (child_r j);
                        Hashtbl.replace h xcr j;
                        sift_down k a h (child_r j)
                | _ -> ()

let rec sift_up k a h j =
        if j > 0 && snd a.(j) < snd a.(parent j) then begin
                let t = a.(j) in
                a.(j) <- a.(parent j);
                a.(parent j) <- t;
                Hashtbl.replace h (fst a.(parent j)) (parent j);
                Hashtbl.replace h (fst a.(j)) j;
                sift_up k a h (parent j)
        end

let push k a h x p =
        assert (Array.length a > !k);
        incr k;
        a.(!k-1) <- (x, p);
        Hashtbl.replace h x (!k-1);
        sift_up k a h (!k-1)

let pop k a h =
        let res = a.(0) in
        decr k;
        a.(0) <- a.(!k);
        Hashtbl.replace h (fst a.(0)) 0;
        Hashtbl.remove h (fst res);
        sift_down k a h 0;
        res

let update k a h x p =
        let j = Hashtbl.find h x in
        a.(j) <- (x, p);
        sift_down k a h j;
        sift_up k a h j

let dij n adj x =
        let res = Array.make n (-1) in
        let a = Array.make n (0, 0) in
        let k = ref 0 in
        let h = Hashtbl.create n in
        push k a h x 0;
        res.(x) <- 0;
        while !k > 0 do
                let (c, d) = pop k a h in
                List.iter (fun (ngh, w) ->
                        if res.(ngh) = -1 || res.(ngh) > d + w then begin
                                res.(ngh) <- d + w;
                                if Hashtbl.mem h ngh then
                                        update k a h ngh res.(ngh)
                                else
                                        push k a h ngh res.(ngh)
                        end
                ) adj.(c)
        done;
        res

let cte n adj (a, b, w) =
        let sp = dij n adj b in
        if sp.(a) = -1 then -1 else sp.(a)+ w


let main s =
        let ic = Scanf.Scanning.from_string s in
        let k = read_int ic in
        for _ = 0 to k-1 do
                let n = read_int ic in
                let m = read_int ic in
                let adj = Array.make n [] in
                let fe = ref (0, 0, 0) in
                for i = 0 to m-1 do
                        let a = read_int ic in
                        let b = read_int ic in
                        let w = read_int ic in
                        if i = 0 then fe := (a-1, b-1, w);
                        adj.(a-1) <- (b-1, w) :: adj.(a-1)
                done;
                Printf.printf "%d " (cte n adj !fe)
        done


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
