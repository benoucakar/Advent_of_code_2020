(* Tu sem sam rešil samo prvi del. Optimizirano koda za 2. del prilagijena po https://www.reddit.com/r/adventofcode/comments/kdf85p/2020_day_15_solutions/*)

let igra vsebina_datoteke cilj = 
    let zacetek = vsebina_datoteke |> String.trim |> String.split_on_char ',' |> List.map int_of_string in
    let ne_zadnji = zacetek |> List.rev |> List.tl |> List.rev in
    let zadnji = zacetek |> List.rev |> List.hd in
    let spomin = Array.make cilj 0 in
    List.iteri (fun i n -> spomin.(n) <- i + 1) ne_zadnji;
    let rec krog poteza prejsni = 
        if poteza = cilj + 1 then prejsni else 
        let naslednji = 
        match spomin.(prejsni) with
        | 0 -> 0
        | j -> poteza - j - 1
        in
        spomin.(prejsni) <- poteza - 1;
        krog (poteza + 1) naslednji
    in
    krog ((List.length zacetek) + 1) zadnji

let naloga1 vsebina_datoteke =
    igra vsebina_datoteke 2020 |> string_of_int

let naloga2 vsebina_datoteke =
    igra vsebina_datoteke 30000000 |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_15.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_15_1.out" odgovor1;
    izpisi_datoteko "day_15_2.out" odgovor2