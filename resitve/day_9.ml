(* Ocaml 4.10.0 še nima List.filteri *)
let filter_range_25 seznam indx =
    List.mapi (fun i n -> (i, n)) seznam |>
    List.filter (fun (i, _) -> i >= (indx-25) && i < indx) |> List.map snd

let vsebina_v_podatke vsebina_datoteke = 
    vsebina_datoteke |> String.trim |> String.split_on_char '\n' |> List.map int_of_string

let slabo_stevilo stevila =
    let rec is_sum indx = 
        if indx < 25 then failwith "Ne ..." else
        if indx >= List.length stevila then failwith "Ni ga takega stevila." else
        let target = List.nth stevila indx
        and kandidati = filter_range_25 stevila indx in
        let rec poisci_par = function
                | [] -> false
                | n::ns -> if List.exists (fun x -> x = (target - n)) ns then true else poisci_par ns
        in
        if poisci_par kandidati then is_sum (indx+1) else target
    in
    is_sum 25

let naloga1 vsebina_datoteke =
    vsebina_datoteke |> vsebina_v_podatke |> slabo_stevilo |> string_of_int

let naloga2 vsebina_datoteke =
    let stevila = vsebina_v_podatke vsebina_datoteke in
    let cilj  = slabo_stevilo stevila in
    let rec sestej_do_cilja indx running_indx spomin vrednost_spomina =
        let trenutni_clen = List.nth stevila (indx + running_indx) in
        let nov_spomin = trenutni_clen :: spomin
        and nova_vsota_spomina = vrednost_spomina + trenutni_clen  in
        if nova_vsota_spomina > cilj then sestej_do_cilja (indx+1) 0 [] 0
        else if nova_vsota_spomina < cilj then sestej_do_cilja indx (running_indx+1) nov_spomin nova_vsota_spomina else
        let urejen_spomin = List.sort compare nov_spomin in
        (List.nth urejen_spomin 0) + (List.nth urejen_spomin ((List.length nov_spomin)-1))
    in
    sestej_do_cilja 0 0 [] 0 |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_9.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_9_1.out" odgovor1;
    izpisi_datoteko "day_9_2.out" odgovor2