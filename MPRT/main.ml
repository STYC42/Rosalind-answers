open Lwt

exception Network_error

type motif = | Only of char 
             | Either of (char * char)
             | Except of char

let fetch_fasta_uniprot entry_id =
  let base_url = "https://rest.uniprot.org/uniprotkb/" ^ entry_id ^ ".fasta" in
  let rec fetch url =
    Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
        Cohttp_lwt.Body.to_string body >>= fun fasta ->
        let filename = entry_id ^ ".fasta" in
        Lwt_io.with_file ~mode:Lwt_io.Output filename (fun oc ->
          Lwt_io.write oc fasta
        ) >>= fun () ->
        Lwt.return_unit
    | `See_other ->
        begin match Cohttp.Header.get (Cohttp.Response.headers resp) "location" with
        | Some location ->
            let redirect_uri =
              try Uri.of_string location with _ ->
                failwith ("URI invalide dans Location : " ^ location)
            in
            let resolved_uri =
              if Uri.scheme redirect_uri = None then
                Uri.resolve "" (Uri.of_string url) redirect_uri
              else
                redirect_uri
            in
            fetch (Uri.to_string resolved_uri)
        | None ->
            Lwt_io.eprintf "Erreur 303 sans en-tête Location : %s\n" url >>= fun () ->
            Lwt.fail Network_error
        end
    | _ ->
        Lwt_io.eprintf "Erreur HTTP : %s (lors du fetch de %s)\n"
          (Cohttp.Code.string_of_status (Cohttp.Response.status resp))
          url >>= fun () ->
        Lwt.fail Network_error
  in
  Lwt_main.run (fetch base_url)
            
            
            


let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let read_fasta filename =
  let ic = open_in filename in
  let rec read_lines acc current_id current_seq =
    try
      let line = input_line ic in
      if String.length line > 0 && line.[0] = '>' then
        let id = String.sub line 1 (String.length line - 1) in
        let acc =
          match current_id with
          | Some id -> (id, String.concat "" (List.rev current_seq)) :: acc
          | None -> acc
        in
        read_lines acc (Some id) []
      else
        read_lines acc current_id (line :: current_seq)
    with End_of_file ->
      let acc =
        match current_id with
        | Some id -> (id, String.concat "" (List.rev current_seq)) :: acc
        | None -> acc
      in
      close_in ic;
      List.rev acc
  in
  read_lines [] None []

let corr_char m c =
  match m with
  | Only k when c = k -> true
  | Except k when c <> k -> true
  | Either (k, _l) when c = k -> true
  | Either (_k, l) when c = l -> true
  | _ -> false

let corr_string m s =
  let res = ref true in
  String.iteri (
    fun i c -> 
      res := !res && corr_char m.(i) c
  ) s;
  !res

let search_motif s =
  let motif = [| Only 'N';
                 Except 'P';
                 Either ('S', 'T');
                 Except 'P' |] in
  let n = String.length s in
  let p = Array.length motif in
  let res = ref [] in
  for i = 0 to n-p do
    if corr_string motif (String.sub s i p) then
      res := (i+1) :: !res
  done;
  List.rev !res


let main s =
  let ids = String.split_on_char '\n' s in
  List.iter (
    fun ci ->
      let u = String.split_on_char '_' ci in
      let id = List.hd u in
      fetch_fasta_uniprot id;
      let f = read_fasta (id ^ ".fasta") in
      if List.length f > 0 then begin
        let str = f |> List.hd |> snd in
        let pos = search_motif str in
        if List.length pos > 0 then begin
          Printf.printf "%s\n" ci;
          List.iter (
            Printf.printf "%d "
          ) pos;
          print_newline ()
        end
      end
  ) ids


let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ./run.out <input_file>";
    exit 1
  end;
  let input = read_file Sys.argv.(1) in
  main input
