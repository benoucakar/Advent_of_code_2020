let vsebina_v_podatke vsebina_datoteke = 
    vsebina_datoteke |> String.trim |> String.split_on_char '\n' 
    |> List.map int_of_string |> List.sort compare

let naloga1 vsebina_datoteke =
    let podatki = vsebina_v_podatke vsebina_datoteke in
    let rec jolt dif1 dif3 trenutni = function
        | [] -> dif1 * (dif3+1) (*Upoštevamo napravo*)
        | n::ns -> 
        match (n-trenutni) with
            | 1 -> jolt (dif1+1) dif3 n ns
            | 3 -> jolt dif1 (dif3+1) n ns
            | _ -> failwith "Nekaj ni vredu."
    in
    jolt 0 0 0 podatki |> string_of_int


let naloga2 vsebina_datoteke =
    let podatki = 0::(vsebina_v_podatke vsebina_datoteke) in
    let rec bloki acc pomozni_acc = function
        | [] -> pomozni_acc::acc
        | n::ns -> 
        if pomozni_acc = [] then bloki acc [n] ns else
        if n - (List.hd pomozni_acc) < 3 then bloki acc (n::pomozni_acc) ns else
        bloki (pomozni_acc::acc) [n] ns
    in
    let moznosti = function
        | 3 -> 2
        | 4 -> 4
        | 5 -> 7
        | _ -> 1 (* Predpostavljam, da je največja dolžina bloka 5 *)
    in
    bloki [] [] podatki |> List.map (fun l -> moznosti (List.length l)) 
    |> List.fold_left (fun x y -> x * y) 1 |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_10.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_10_1.out" odgovor1;
    izpisi_datoteko "day_10_2.out" odgovor2