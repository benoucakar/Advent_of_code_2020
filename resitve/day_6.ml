let vsebina_v_podatke vsebina_datoteke = 
    let podatki = vsebina_datoteke |> String.trim |> String.split_on_char '\n' in
    let rec aux glavni_sez pomozni_sez = function
        | [] -> pomozni_sez :: glavni_sez
        | glava::rep -> 
        if glava = "" then aux (pomozni_sez :: glavni_sez) [] rep
        else aux glavni_sez (glava :: pomozni_sez) rep
    in
    aux [] [] podatki

let resitev vsebina_datoteke list_exists_ali_for_all =
    let podatki = vsebina_v_podatke vsebina_datoteke in
    let rec aux acc = function
        | [] -> acc 
        | glava::rep -> 
        let crke = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'] in
        let rec stevec sez_niz acc' = function
            | [] -> acc'
            | c::cs -> 
            if list_exists_ali_for_all (fun niz -> String.contains niz c) sez_niz 
            then stevec sez_niz (acc' + 1) cs else stevec sez_niz acc' cs  
        in
        aux (acc + (stevec glava 0 crke)) rep
    in
    aux 0 podatki |> string_of_int

let naloga1 vsebina_datoteke =
    resitev vsebina_datoteke List.exists

let naloga2 vsebina_datoteke =
    resitev vsebina_datoteke List.for_all

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
    let vsebina_datoteke = preberi_datoteko "day_6.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_6_1.out" odgovor1;
    izpisi_datoteko "day_6_2.out" odgovor2