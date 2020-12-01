let seznam_str_to_int vsebina_datoteke = 
    let seznam = String.split_on_char '\n' vsebina_datoteke in
    seznam |> List.filter (fun s -> s <> "") |> List.map int_of_string

let par_do_vsote vsota sez = 
    let rec poisci_par = function
            | [] -> None
            | n::ns -> 
                match List.find_opt (fun x -> x = (vsota - n)) ns with
                | None -> poisci_par ns
                | Some m -> Some (m, n)
        in
        poisci_par sez

let naloga1 vsebina_datoteke =
    let sez_stevil = seznam_str_to_int vsebina_datoteke in
    match par_do_vsote 2020 sez_stevil with
    | None -> "Ni para"
    | Some (m, n) -> string_of_int (m * n)

let naloga2 vsebina_datoteke =
    let sez_stevil = seznam_str_to_int vsebina_datoteke in
    let rec poisci_trojico = function
        | [] -> "Ni trojice"
        | n::ns -> 
        match par_do_vsote (2020 - n) ns with
            | None -> poisci_trojico ns
            | Some (a, b) -> string_of_int (a * b * n)
    in
    poisci_trojico sez_stevil

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
    let vsebina_datoteke = preberi_datoteko "day_1.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_1_1.out" odgovor1;
    izpisi_datoteko "day_1_2.out" odgovor2