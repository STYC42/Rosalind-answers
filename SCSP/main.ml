let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let lcsq s1 s2 =
        let n = String.length s1 in
        let m = String.length s2 in
        let rs1 j = s1.[n-j-1] in
        let rs2 j = s2.[m-j-1] in
        let h = Hashtbl.create 1 in
        let rec dyn i j =
                match Hashtbl.find_opt h (i, j) with
                | Some l -> l
                | None -> let res = aux i j in
                          Hashtbl.add h (i, j) res; res
        and aux i j =
                if i = 0 then 
                        match String.index_opt (String.sub s2 (m-j-1) (j+1)) s1.[n-1] with
                        | None -> []
                        | Some _ -> [[s1.[n-1]]]
                else if j = 0 then
                        match String.index_opt (String.sub s1 (n-i-1) (i+1)) s2.[m-1] with
                        | None -> []
                        | Some _ -> [[s2.[m-1]]]
                else if rs1 i = rs2 j then
                        let hr = dyn (i-1) (j-1) in
                        match hr with
                        | [] -> [[rs1 i]]
                        | _ -> List.map (fun l -> rs1 i :: l) hr |> List.hd |> fun i -> [i]
                else
                        let t1 = dyn i (j-1) in
                        let n1 = List.length (match t1 with [] -> [] | h :: _ -> h) in
                        let t2 = dyn (i-1) j in
                        let n2 = List.length (match t2 with [] -> [] | h :: _ -> h) in
                        if n1 > n2 then t1
                        else if n2 > n1 then t2
                        else t1 @ t2
        in 
        dyn (n-1) (m-1) |> List.hd
                |> List.map (String.make 1)
                |> String.concat ""

let main s =
        let ic = Scanf.Scanning.from_string s in
        let s1 = Scanf.bscanf ic " %s" (fun i -> i) ^ " " in
        let s2 = Scanf.bscanf ic " %s" (fun i -> i) ^ " "in
        let lc = lcsq s1 s2 ^ " " in
        let i = ref 0 in
        let j = ref 0 in
        let s = ref 0 in
        while !i < String.length s1 && !j < String.length s2 do
                if s1.[!i] <> lc.[!s] then begin
                        print_char s1.[!i];
                        incr i
                end else if s2.[!j] <> lc.[!s] then begin
                        print_char s2.[!j];
                        incr j
                end else begin
                        print_char lc.[!s];
                        incr i; incr j; incr s
                end
        done


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
