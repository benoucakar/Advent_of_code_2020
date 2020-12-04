let vsebina_v_podatke vsebina_datoteke = 
    let grd_sez vsebina_datoteke = vsebina_datoteke |> String.trim |> String.split_on_char ' ' in
    let lep_sez = List.fold_left (fun acc niz -> acc @ (String.split_on_char '\n' niz)) [] (grd_sez vsebina_datoteke) in
    let rec aux veliki_sez pomozni_sez = function
        | [] -> pomozni_sez :: veliki_sez
        | niz::rep ->
        let kljuc niz' = List.nth (String.split_on_char ':' niz') 0
        and vred niz' = List.nth (String.split_on_char ':' niz') 1 in
        if niz = "" then aux (pomozni_sez :: veliki_sez) [] rep
        else aux veliki_sez ((kljuc niz, vred niz):: pomozni_sez) rep
    in
    aux [] [] lep_sez

let preizkus (kljuc, vred) = 
    let v_intervalu n sp_meja zg_meja = n >= sp_meja && n <= zg_meja in
    let dolzina_vred = String.length vred in
    match kljuc with
    | "byr" -> v_intervalu (int_of_string vred) 1920 2002
    | "iyr" -> v_intervalu (int_of_string vred) 2010 2020
    | "eyr" -> v_intervalu (int_of_string vred) 2020 2030
    | "hgt" ->
        let enota = vred.[dolzina_vred - 1] in
        let kolicina = (String.sub vred 0 (dolzina_vred - 2)) in
        ((enota = 'm') && (v_intervalu (int_of_string kolicina) 150 193))
        ||
        ((enota = 'n') && (v_intervalu (int_of_string kolicina) 59 76))
    | "hcl" -> 
        let dobri_znaki = "abcdef0123456789" in
        dolzina_vred = 7 && (List.for_all (fun n -> String.contains dobri_znaki vred.[n]) [1; 2; 3; 4; 5; 6])
    | "ecl" -> List.mem vred ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
    | "pid" -> dolzina_vred = 9
    | "cid" -> true
    | _ -> false

let naloga1 vsebina_datoteke =
    let podatki = vsebina_v_podatke vsebina_datoteke in
    let preizkus sez = (List.length sez = 8) || (((List.length sez) = 7) && not(List.mem "cid" (List.map fst sez))) in
    let rec aux stevec = function
        | [] -> stevec
        | l::ls -> if preizkus l then aux (stevec+1) ls else aux stevec ls
    in
    string_of_int (aux 0 podatki)

let naloga2 vsebina_datoteke =
    let podatki = vsebina_v_podatke vsebina_datoteke in
    let preizkusevalec sez = 
        if (List.length sez = 8) || (((List.length sez) = 7) && not(List.mem "cid" (List.map fst sez))) 
        then List.for_all preizkus sez else false
    in
    let rec aux stevec = function
        | [] -> stevec
        | g::rep -> if preizkusevalec g then aux (stevec+1) rep else aux stevec rep
    in
    string_of_int (aux 0 podatki)

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
    let vsebina_datoteke = preberi_datoteko "day_4.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_4_1.out" odgovor1;
    izpisi_datoteko "day_4_2.out" odgovor2