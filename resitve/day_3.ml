let vsebina_v_sez vsebina_datoteke = 
    vsebina_datoteke |> String.trim |> String.split_on_char '\n'

let trki_na_poti korak_x korak_y proga = 
    let sirina = String.length (List.nth proga 0)
    and dolzina = List.length proga in
    let premik (x, y) = (((x + korak_x) mod sirina), y + korak_y) in
    let rec stevec (x, y) acc = 
        if y >= dolzina then acc else
        let vrstica = List.nth proga y in
        if (String.get vrstica x) = '#' then stevec (premik (x, y)) (acc+1) else stevec (premik (x, y)) acc
    in
    stevec (0, 0) 0

let naloga1 vsebina_datoteke =
    let proga = vsebina_v_sez vsebina_datoteke in
    string_of_int (trki_na_poti 3 1 proga)

let naloga2 vsebina_datoteke =
    let proga = vsebina_v_sez vsebina_datoteke in
    let trki1 = trki_na_poti 1 1 proga
    and trki2 = trki_na_poti 3 1 proga
    and trki3 = trki_na_poti 5 1 proga
    and trki4 = trki_na_poti 7 1 proga
    and trki5 = trki_na_poti 1 2 proga in
    string_of_int (trki1 * trki2 * trki3 * trki4 * trki5)

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
    let vsebina_datoteke = preberi_datoteko "day_3.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_3_1.out" odgovor1;
    izpisi_datoteko "day_3_2.out" odgovor2