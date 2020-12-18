let racunalo navodila = 
    let loci_na_zaklepaju sez =
        let rec aux acc mera = function
            | [] -> failwith "To se nam ne bo zgodilo."
            | c::cs -> 
            match c with
                | '(' -> aux (c::acc) (mera+1) cs
                | ')' when mera-1 = 0 -> (List.rev acc, cs)
                | ')' -> aux (c::acc) (mera-1) cs
                | _ -> aux (c::acc) mera cs
        in
        aux [] 1 sez
    in
    let chr_to_int c = String.make 1 c |> int_of_string in
    let rec aux acc operacija = function
            | [] -> acc
            | c::cs ->
            match c with
                | '+' -> aux acc (+) cs
                | '*' -> aux acc ( * ) cs
                | '(' -> 
                    let (pred, za) = loci_na_zaklepaju cs in
                    aux (operacija acc (aux 0 ( + ) pred)) operacija za
                | stevilo -> aux (operacija acc (chr_to_int stevilo)) operacija cs
    in
    List.fold_left (+) 0 (List.map (aux 0 ( + )) navodila)

let naloga1 vsebina_datoteke =
    let navodila = vsebina_datoteke |> String.trim |> String.split_on_char '\n' 
    |> List.map (fun niz -> List.init (String.length niz) (fun i -> niz.[i])) 
    |> List.map (List.filter (fun c -> c <> ' ')) in
    racunalo navodila |> string_of_int

let naloga2 vsebina_datoteke =
    let popravi niz = 
        let notr = List.init (String.length niz) (fun i -> 
            match niz.[i] with
                | '+' -> ")+("
                | '*' -> "))*(("
                | '(' -> "(("
                | ')' -> "))"
                | c -> String.make 1 c)
        |> String.concat "" in
        "(((" ^ notr ^ ")))"
    in
    let navodila = vsebina_datoteke |> String.trim |> String.split_on_char '\n' 
    |> List.map popravi |> List.map (fun niz -> List.init (String.length niz) (fun i -> niz.[i])) 
    |> List.map (List.filter (fun c -> c <> ' ')) in
    racunalo navodila |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_18.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_18_1.out" odgovor1;
    izpisi_datoteko "day_18_2.out" odgovor2