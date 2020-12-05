let particija_v_id particija =
    let rec aux vrsta stolpec mesto_vrst mesto_stolp = function
        | [] -> mesto_vrst * 8 + mesto_stolp
        | c::cs ->
            match c with
                | 'F' -> aux (vrsta / 2) stolpec mesto_vrst mesto_stolp cs
                | 'B' -> aux (vrsta / 2) stolpec (mesto_vrst + (vrsta / 2)) mesto_stolp cs
                | 'L' -> aux vrsta (stolpec / 2) mesto_vrst mesto_stolp cs
                | 'R' -> aux vrsta (stolpec / 2) mesto_vrst (mesto_stolp + (stolpec / 2)) cs
                | _ -> 0
    in
    aux 128 8 0 0 particija

let vsebina_v_podatke vsebina_datoteke = 
    let podatki vsebina_datoteke = vsebina_datoteke |> String.trim |> String.split_on_char '\n' in
    let loci_crke niz = List.init 10 (fun i -> niz.[i]) in
    let rec aux acc = function
        | [] -> acc
        | l::ls -> aux ((loci_crke l) :: acc) ls
    in
    aux [] (podatki vsebina_datoteke) |> List.map particija_v_id


let naloga1 vsebina_datoteke =
    let rec max_int = function
        | [] -> 0
        | x::xs -> max x (max_int xs)
    in
    vsebina_datoteke |> vsebina_v_podatke |> max_int |> string_of_int

let naloga2 vsebina_datoteke =
    let rec iskalec_lunknje = function
        | [] -> 0
        | n::[] -> 0
        | n::ns -> if ((n+1) = List.nth ns 0) then iskalec_lunknje ns else (n+1)
    in
    vsebina_datoteke |> vsebina_v_podatke |> List.sort compare |> iskalec_lunknje |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_5.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_5_1.out" odgovor1;
    izpisi_datoteko "day_5_2.out" odgovor2