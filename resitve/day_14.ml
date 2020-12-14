(*Ta stvar deluje. Zelo počasi, ampak deluje.*)

type ukaz = 
    | Mask of (char list)
    | Write of (int * int)

let to_bin x =
    let converter n = if n = 1 then '1' else '0' in
    let rec aux n acc =
        if n = 0 then acc
        else aux (n/2) (((n mod 2)|> converter) ::acc)
    in
    let stevilo = aux x [] in
    (List.init (36 - List.length stevilo) (fun i -> '0')) @ stevilo

let from_bin l =
    let rec aux potenca acc = function
        | [] -> acc
        | c::cs -> if c = '1' then aux (2 * potenca) (acc + potenca) cs else aux (2 * potenca) acc cs
    in
    aux 1 0 (List.rev l) 

let vsebina_v_podatke vsebina_datoteke = 
    vsebina_datoteke |> String.trim |> String.split_on_char '\n' |> List.map (fun niz -> 
        if niz.[1] = 'a' then Mask (
            List.init (String.length niz) (fun i -> if i > 6 then niz.[i] else '-') |> List.filter (fun c -> c <> '-') 
            )
        else Write (
            String.sub niz 4 ((String.index niz ']') - 4) |> int_of_string,
            List.nth (String.split_on_char ' ' niz) 2 |> int_of_string
            )
        )

let zamenjaj_prvi a b sez = 
    let rec aux acc = function
        | [] -> List.rev acc
        | x::xs -> if x = a then (List.rev (b::acc)) @ xs else aux (x::acc) xs
    in
    aux [] sez

let naloga1 vsebina_datoteke =
    let ukazi = vsebina_datoteke |> vsebina_v_podatke in
    let apply_mask maska bin = List.map2 (fun m b -> if m = 'X' then b else m) maska bin in
    let rec izvajanje spomin maska = function
        | [] -> spomin
        | u::us ->
        match u with
            | Mask m -> izvajanje spomin m us
            | Write (n, x) -> 
            let nov_x = x |> to_bin |> apply_mask maska |> from_bin in
            match List.find_opt (fun (a, _) -> a = n) spomin with
                | None -> izvajanje ((n, nov_x)::spomin) maska us
                | Some y -> izvajanje (zamenjaj_prvi y (n, nov_x) spomin) maska us
    in 
    let koncen_spomin = izvajanje [] [] ukazi in
    List.fold_left (+) 0 (List.map (fun (_, x) -> x) koncen_spomin) |> string_of_int

let naloga2 vsebina_datoteke =
    let ukazi = vsebina_datoteke |> vsebina_v_podatke in
    let apply_mask maska bin = 
        let template = List.map2 (fun m b -> if m = '0' then b else if m = '1' then '1' else 'X') maska bin in
        let rec obdelava obdelani = function
            | [] -> obdelani
            | l::ls ->
                if List.mem 'X' l then obdelava obdelani ((zamenjaj_prvi 'X' '1' l)::(zamenjaj_prvi 'X' '0' l)::ls)
                else obdelava (l::obdelani) ls
        in
        obdelava [] [template]
    in
    let rec izvajanje spomin maska = function
        | [] -> spomin
        | u::us ->
        match u with
            | Mask m -> izvajanje spomin m us
            | Write (n, x) -> 
            let za_napisat = n |> to_bin |> apply_mask maska |> List.map (fun bin -> (from_bin bin, x)) in
            let rec aux nov_spomin = function
                | [] -> izvajanje nov_spomin maska us 
                | (n, x)::rep ->
                match List.find_opt (fun (a, _) -> a = n) nov_spomin with
                | None -> aux ((n, x)::nov_spomin) rep
                | Some y -> aux (zamenjaj_prvi y (n, x)nov_spomin) rep
            in
            aux spomin za_napisat
    in 
    let koncen_spomin = izvajanje [] [] ukazi in
    List.fold_left (+) 0 (List.map (fun (_, x) -> x) koncen_spomin) |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_14.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_14_1.out" odgovor1;
    izpisi_datoteko "day_14_2.out" odgovor2