(* Implementacija KIO prilagojena po https://rosettacode.org/wiki/Chinese_remainder_theorem#OCaml *)

let rec egcd a b =
    if b = 0 then (1, 0) else
    let q = a / b and r = a mod b in
    let (s, t) = egcd b r in
    (t, s - q * t)

let mod_inv a b =
    let (x, y) = egcd a b in
    if a * x + b * y = 1 then Some x else None

let calc_inverses ns ms =
    let rec list_inverses ns ms l =
        match (ns, ms) with
        | ([], []) -> Some l
        | ([], _) | (_, []) -> failwith "Slabi podatki!"
        | (n::ns, m::ms) ->
            let inv = mod_inv n m in
                match inv with
                | None -> None
                | Some v -> list_inverses ns ms (v::l)
    in
    match list_inverses ns ms [] with
        | None -> None
        | Some l -> Some (List.rev l)

let unzip sez = 
    let rec aux l1 l2 = function
        | [] -> (l1, l2)
        | (x,y)::rep -> aux (x::l1) (y::l2) rep
    in
    aux [] [] sez

let chinese_remainder congruences =
    let (residues, modulii) = unzip congruences in
    let mod_pi = List.fold_left ( * ) 1 modulii in
    let crt_modulii = List.map (fun m -> mod_pi / m) modulii in
        match calc_inverses crt_modulii modulii with 
            | None -> None
            | Some inverses ->
                Some (List.map2 ( * ) (List.map2 ( * ) residues inverses) crt_modulii
                |> List.fold_left (+) 0 |> fun n -> let n' = n mod mod_pi in if n' < 0 then n' + mod_pi else n')

let naloga1 vsebina_datoteke =
    let vrstici = vsebina_datoteke |> String.trim |> String.split_on_char '\n' in
    let est = List.nth vrstici 0 |> int_of_string 
    and ids = List.nth vrstici 1 |> String.split_on_char ',' |> List.filter (fun c -> c <> "x") |> List.map int_of_string in
    let rec zamuda n acc = if acc < est then zamuda n (acc + n) else acc - est in
    ids |> List.mapi (fun i n -> (zamuda n 0, List.nth ids i)) |> List.sort compare |> List.hd |> fun (x, y) -> x * y |> string_of_int

let naloga2 vsebina_datoteke =
    let vrstici = vsebina_datoteke |> String.trim |> String.split_on_char '\n' in
    let ids = List.nth vrstici 1 |> String.split_on_char ',' |>
    List.mapi (fun i c -> if c = "x" then (0,0) else (-i, int_of_string c)) |> List.filter (fun (j, n) -> n <> 0) in
    match chinese_remainder ids with
        | None -> failwith "Se ne ujamejo"
        | Some n -> string_of_int n

let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko "day_13.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_13_1.out" odgovor1;
    izpisi_datoteko "day_13_2.out" odgovor2