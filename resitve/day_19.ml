type pravilo =
    | Crka of (string)
    | Kazalec of (int list list)

let vsebina_v_podatke vsebina_datoteke = 
    let input = vsebina_datoteke |> String.trim |> String.split_on_char '\n' in
    let v_pravilo niz = 
        let temp = String.split_on_char ':' niz in
        let indx = List.hd temp |> int_of_string in
        if List.nth temp 1 = " \"a\"" then (indx, Crka "a") else 
        if List.nth temp 1 = " \"b\"" then (indx, Crka "b") else
        let pointers = List.nth temp 1 |> String.split_on_char '|' 
        |> List.map (fun niz -> niz |> String.trim |> String.split_on_char ' ' |> List.map (fun s -> int_of_string s)) in
        (indx, Kazalec pointers)
    in
    let rec aux pravila podatki = function
        | [] -> (pravila |> List.map (fun niz -> v_pravilo niz), podatki)
        | x::xs ->
            match x with
                | "" -> aux pravila podatki xs
                | x when x.[0] = 'a' || x.[0] = 'b' -> aux pravila (x::podatki) xs
                | _ -> aux (x::pravila) podatki xs
    in
    aux [] [] input

let naloga1 vsebina_datoteke =
    let (pravila, podatki) = vsebina_datoteke |> vsebina_v_podatke in
    let spomin = Array.make (List.length pravila) (Crka "") in
    List.iter (fun (indx, pravilo) -> spomin.(indx) <- pravilo) pravila;
    let memoizacija = Array.make (List.length pravila) [] in
    let rec aux i =
        if memoizacija.(i) = [] then
            let rezultat = ref [] in
            match spomin.(i) with
                | Crka s -> 
                    rezultat := [s];
                    memoizacija.(i) <- (!rezultat);
                    !rezultat
                | Kazalec l ->
                    let produkt sez1 sez2 = 
                        let rec mnozenje acc = function
                            | [] -> acc
                            | x::xs -> mnozenje (List.rev_append (List.map (fun niz -> x ^ niz) sez2) acc) xs
                        in
                        mnozenje [] sez1
                    in
                    let obravnavaj = function
                        | [j] -> aux j
                        | [j; k] -> produkt (aux j) (aux k)
                        | [j; k; m] -> produkt (produkt (aux j) (aux k)) (aux m)
                        | _ -> failwith "Tega ne bo 1."
                    in
                    match l with    
                        | [a] ->
                            rezultat := (obravnavaj a);
                            memoizacija.(i) <- (!rezultat);
                            !rezultat
                        | [a;b] -> 
                            rezultat := (List.rev_append (obravnavaj a) (obravnavaj b));
                            memoizacija.(i) <- (!rezultat);
                            !rezultat
                        | _ -> failwith "Tega ne bo 2."
        else memoizacija.(i)
    in
    let dobri_nizi = aux 0 in
    List.fold_left (fun acc niz -> if List.mem niz dobri_nizi then acc + 1 else acc) 0 podatki |> string_of_int

let naloga2 vsebina_datoteke =
    "Nope"

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
    let vsebina_datoteke = preberi_datoteko "day_19.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_19_1.out" odgovor1;
    izpisi_datoteko "day_19_2.out" odgovor2