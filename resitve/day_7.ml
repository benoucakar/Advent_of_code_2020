(* Ko bi znal uporabljat regularne izraze :/ *)

let vsebina_v_podatke vsebina_datoteke = 
    let podatki = vsebina_datoteke |> String.trim |> String.split_on_char '\n' in
    let pravilo_v_nahrbtnik pravilo =  
        let besede = String.split_on_char ' ' pravilo in
        let pomozna = function
            | [] | [_] | [_; _] | [_; _; _] -> failwith "Slabo pravilo ..."
            | n1::n2::_::_::ostalo -> 
            let ime = n1 ^ " " ^ n2 in
            let rec aux acc = function
                | [] | [_] | [_; _] | [_; _; _] -> acc (* Poskrbi za "no other bags." *)
                | st::m1::m2::_::rep -> aux ((m1 ^ " " ^ m2, int_of_string st) :: acc) rep
            in
            (ime, aux [] ostalo)
        in pomozna besede
    in
    List.map pravilo_v_nahrbtnik podatki


let naloga1 vsebina_datoteke =
    let bags = vsebina_v_podatke vsebina_datoteke in
    let vsebujejo ime sez_nahrbtnikov =
        let rec aux acc = function
           | [] -> acc
           | b::bs ->
           let subbags = snd b |> List.map fst in
           if List.exists (fun i -> i = ime) subbags then aux ((fst b)::acc) bs
           else aux acc bs
        in
        aux [] sez_nahrbtnikov
    in
    let prvi_starsi = vsebujejo "shiny gold" bags in
    let rec stevec acc spomin = function
        | [] -> acc
        | b::bs ->
        if List.exists (fun x -> x = b) spomin then stevec acc spomin bs
        else stevec (acc+1) (b::spomin) (bs @ (vsebujejo b bags))
    in
    stevec 0 [] prvi_starsi |> string_of_int

let naloga2 vsebina_datoteke =
    let bags = vsebina_v_podatke vsebina_datoteke in
    let rec st_vseb_nahrbtnikov ime = 
        let subbags = snd (List.find (fun n -> ime = fst n) bags) in
        List.fold_left (+) 0 (List.map (fun (ime, st) -> (st + st * st_vseb_nahrbtnikov ime)) subbags)
    in 
    st_vseb_nahrbtnikov "shiny gold" |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_7.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_7_1.out" odgovor1;
    izpisi_datoteko "day_7_2.out" odgovor2