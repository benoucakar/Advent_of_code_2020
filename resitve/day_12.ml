let vsebina_v_podatke vsebina_datoteke = 
    let v_pravilo niz = (niz.[0], String.sub niz 1 (String.length niz - 1) |> int_of_string) in
    vsebina_datoteke |> String.trim |> String.split_on_char '\n' |> List.map v_pravilo

let mod_360 n =
    if n >= 0 then n mod 360
    else (n mod 360) + 360

let naloga1 vsebina_datoteke =
    let ukazi = vsebina_v_podatke vsebina_datoteke in
    let rec premik x y smer = function
        | [] -> (Int.abs x) + (Int.abs y)
        | (u, n)::rep ->
        match u with
            | 'N' -> premik x (y + n) smer rep
            | 'S' -> premik x (y - n) smer rep
            | 'E' -> premik (x + n) y smer rep
            | 'W' -> premik (x - n) y smer rep
            | 'L' -> premik x y (mod_360 (smer + n)) rep
            | 'R' -> premik x y (mod_360 (smer - n)) rep
            | 'F' when smer = 90 -> premik x (y + n) smer rep
            | 'F' when smer = 270 -> premik x (y - n) smer rep
            | 'F' when smer = 0 -> premik (x + n) y smer rep
            | 'F' when smer = 180 -> premik (x - n) y smer rep
            | _ -> failwith "Neveljaven ukaz!"
    in
    premik 0 0 0 ukazi |> string_of_int

let naloga2 vsebina_datoteke =
    let ukazi = vsebina_v_podatke vsebina_datoteke in
    let rec premik x y way_x way_y = function
        | [] -> (Int.abs x) + (Int.abs y)
        | (u, n)::rep ->
        match u with
            | 'N' -> premik x y way_x (way_y + n) rep
            | 'S' -> premik x y way_x (way_y - n) rep
            | 'E' -> premik x y (way_x + n) way_y rep
            | 'W' -> premik x y (way_x - n) way_y rep
            | 'L' when n = 90 -> premik x y (- way_y) way_x rep 
            | 'L' when n = 180 -> premik x y (- way_x) (- way_y) rep 
            | 'L' when n = 270 -> premik x y way_y (- way_x) rep
            | 'R' when n = 90 -> premik x y way_y (- way_x) rep
            | 'R' when n = 180 -> premik x y (- way_x) (- way_y) rep
            | 'R' when n = 270 -> premik x y (- way_y) way_x rep
            | 'F' -> premik (x + n * way_x) (y + n * way_y) way_x way_y rep
            | _ -> failwith "Neveljaven ukaz!"
    in
    premik 0 0 10 1 ukazi |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_12.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_12_1.out" odgovor1;
    izpisi_datoteko "day_12_2.out" odgovor2