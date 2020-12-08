let vsebina_v_podatke vsebina_datoteke = 
    let podatki = vsebina_datoteke |> String.trim |> String.split_on_char '\n' in
    let ukaz niz = List.nth (String.split_on_char ' ' niz) 0
    and korak niz = List.nth (String.split_on_char ' ' niz) 1 |> int_of_string in
    List.map (fun niz -> (ukaz niz, korak niz)) podatki

let izvedi_ukaze ukazi =
    let rec izvedi acc spomin indx = 
        let dokoncal_ukaze = indx >= List.length ukazi in
        if (List.mem indx spomin) || dokoncal_ukaze then (acc, dokoncal_ukaze) else
        let (u, n) = List.nth ukazi indx in
        match u with
        | "acc" -> izvedi (acc+n) (indx::spomin) (indx+1)
        | "jmp" -> izvedi acc (indx::spomin) (indx+n)
        | "nop" -> izvedi acc (indx::spomin) (indx+1)
        | _ -> failwith "Kaj je to za en ukaz?"
    in
    izvedi 0 [] 0

let naloga1 vsebina_datoteke =
    vsebina_datoteke |> vsebina_v_podatke |> izvedi_ukaze
    |> fst |> string_of_int

let naloga2 vsebina_datoteke =
    let ukazi = vsebina_v_podatke vsebina_datoteke in
    let rec preveri ind = 
        if fst (List.nth ukazi ind) = "acc" then preveri (ind+1) else
        let menjaj = function
        | ("nop", n) -> ("jmp", n)
        | ("jmp", n) -> ("nop", n)
        | _ -> failwith "Menjava nekaj nagaja ..."
        in
        let spremenjeni_ukazi = List.mapi (fun i u -> if i = ind then menjaj u else u) ukazi in
        let (acc, dokoncal) = izvedi_ukaze spremenjeni_ukazi in
        if dokoncal then acc else preveri (ind+1)
    in
    preveri 0 |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_8.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_8_1.out" odgovor1;
    izpisi_datoteko "day_8_2.out" odgovor2