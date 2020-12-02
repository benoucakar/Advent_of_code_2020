(* "gasche": https://stackoverflow.com/questions/10580420/explanation-of-ocaml-code-explode-a-string-split-a-list *)
let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

let vsebina_v_sez vsebina_datoteke = 
    let seznam = vsebina_datoteke |> String.trim |> String.split_on_char '\n' in
    let rec aux acc = function
        | [] -> acc
        | x::xs ->
        let podatki = String.split_on_char ' ' x in
        let meji = (String.split_on_char '-' (List.nth podatki 0)) in
        let spodnja_meja = int_of_string (List.nth meji 0)
        and zgornja_meja = int_of_string (List.nth meji 1) in
        let crka = (List.nth podatki 1).[0] in
        let geslo = List.nth podatki 2 in
        aux ((spodnja_meja, zgornja_meja, crka, geslo) :: acc) xs
    in
    aux [] seznam

let naloga1 vsebina_datoteke =
    let podatki = vsebina_v_sez vsebina_datoteke in
    let dodaj crka pojavitve x = if (crka = x) then (pojavitve + 1) else pojavitve in
    let rec stevec acc_dobri = function
        | [] -> acc_dobri
        | (sp_meja, zg_meja, crka, geslo)::rep ->
        let pojavitve = List.fold_left (dodaj crka) 0 (explode geslo) in
        if (pojavitve >= sp_meja) && (pojavitve <= zg_meja) then stevec (acc_dobri+1) rep
        else stevec acc_dobri rep
    in
    string_of_int (stevec 0 podatki)

let naloga2 vsebina_datoteke =
    let podatki = vsebina_v_sez vsebina_datoteke in
    let rec stevec acc_dobri = function
        | [] -> acc_dobri
        | (sp_meja, zg_meja, crka, geslo)::rep ->
        let ujemanje1 = geslo.[sp_meja - 1] = crka
        and ujemanje2 = geslo.[zg_meja - 1] = crka in
        if (ujemanje1 && (not ujemanje2)) || ((not ujemanje1) && ujemanje2) then stevec (acc_dobri + 1) rep
        else stevec acc_dobri rep
    in
    string_of_int (stevec 0 podatki)

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
    let vsebina_datoteke = preberi_datoteko "day_2.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_2_1.out" odgovor1;
    izpisi_datoteko "day_2_2.out" odgovor2