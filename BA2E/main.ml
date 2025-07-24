let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let profile_count_laplace profile =
  let n = profile |> List.hd 
                  |> String.length in
  let nbA = Array.make n 1 in
  let nbC = Array.make n 1 in
  let nbG = Array.make n 1 in
  let nbT = Array.make n 1 in
  List.iter (
    String.iteri (
      fun i -> function
        | 'A' -> nbA.(i) <- nbA.(i) + 1
        | 'C' -> nbC.(i) <- nbC.(i) + 1
        | 'G' -> nbG.(i) <- nbG.(i) + 1
        | 'T' -> nbT.(i) <- nbT.(i) + 1
        | _ -> failwith "Character unexpected from a DNA strand"
    )
  ) profile;
  (nbA, nbC, nbG, nbT)

let profile_count profile =
  let n = profile |> List.hd 
                  |> String.length in
  let nbA = Array.make n 0 in
  let nbC = Array.make n 0 in
  let nbG = Array.make n 0 in
  let nbT = Array.make n 0 in
  List.iter (
    String.iteri (
      fun i -> function
        | 'A' -> nbA.(i) <- nbA.(i) + 1
        | 'C' -> nbC.(i) <- nbC.(i) + 1
        | 'G' -> nbG.(i) <- nbG.(i) + 1
        | 'T' -> nbT.(i) <- nbT.(i) + 1
        | _ -> failwith "Character unexpected from a DNA strand"
    )
  ) profile;
  (nbA, nbC, nbG, nbT)

let profile_prob_laplace profile =
  let n = profile |> List.hd |> String.length in
  let nbA, nbC, nbG, nbT = profile_count_laplace profile in
  Array.init n (
    fun i ->
      let tot = float_of_int (nbA.(i) + nbC.(i) + nbG.(i) + nbT.(i)) in
      (float_of_int nbA.(i) /. tot,
       float_of_int nbC.(i) /. tot,
       float_of_int nbG.(i) /. tot,
       float_of_int nbT.(i) /. tot)
  )

let profile_prob profile =
  let n = profile |> List.hd |> String.length in
  let nbA, nbC, nbG, nbT = profile_count profile in
  Array.init n (
    fun i ->
      let tot = float_of_int (nbA.(i) + nbC.(i) + nbG.(i) + nbT.(i)) in
      (float_of_int nbA.(i) /. tot,
       float_of_int nbC.(i) /. tot,
       float_of_int nbG.(i) /. tot,
       float_of_int nbT.(i) /. tot)
  )

let profile_matching kmer prof_prob =
  let res = ref 1. in
  String.iteri (
    fun i ->
      let a, c, g, t = prof_prob.(i) in
      function 
      | 'A' -> res := !res *. a
      | 'C' -> res := !res *. c
      | 'G' -> res := !res *. g
      | 'T' -> res := !res *. t
      | _ -> failwith "Character unexpected from a DNA strand"
  ) kmer;
  !res

let consensus_string profile =
  let n = profile |> List.hd |> String.length in
  let nbA, nbC, nbG, nbT = profile_count profile in
  String.init n (
    fun i ->
      let a, c, g, t = nbA.(i), nbC.(i), nbG.(i), nbT.(i) in
      if a >= c && a >= g && a >= t then
        'A'
      else if c >= g && c >= t then
        'C'
      else if g >= t then
        'G'
      else
        'T'
  )

let profile_most_prob_laplace k profile strand =
  let n = String.length strand in
  let pb = profile_prob_laplace profile in
  let res = ref "" in
  let vres = ref 0. in
  for i = 0 to n - k do
    let kmer = String.sub strand i k in
    let pm = profile_matching kmer pb in
    if pm < !vres then begin
      res := kmer;
      vres := pm
    end
  done;
  !res

let hamm s1 s2 =
  let res = ref 0 in
  String.iteri (
    fun i _c ->
      if s1.[i] <> s2.[i] then incr res
  ) s1;
  !res

let score_strand kmer strand =
  let n = String.length strand in
  let k = String.length kmer in
  let res = ref max_int in
  for i = 0 to n - k do
    let sub = String.sub strand i k in
    let dist = hamm kmer sub in
    res := min !res dist
  done;
  !res

let score_sample kmer dna =
  dna |> List.map (score_strand kmer)
      |> List.fold_left (+) 0


let greedy_motif_search k strands =
  let dna1 = List.hd strands in
  let n = String.length dna1 in
  let res = ref "" in
  let vres = ref max_int in
  for i = 0 to n - k do
    let kmer = String.sub dna1 i k in
    let profile = ref [kmer] in
    List.iter (
      fun strand ->
        profile := profile_most_prob_laplace k !profile strand :: !profile
    ) (List.tl strands);
    let cons = consensus_string !profile in
    let score = score_sample cons strands in
    if score < !vres then begin
      res := cons;
      vres := score
    end
  done;
  !res

let main s =
  (* ... *)
  ()



let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
