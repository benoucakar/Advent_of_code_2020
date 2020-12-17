let vsebina_v_podatke vsebina_datoteke = 
    vsebina_datoteke |> String.trim |> String.split_on_char '\n' 
    |> List.map (fun niz -> List.init (String.length niz) (fun i -> niz.[i]) |> Array.of_list) |> Array.of_list

let okolica_3D ((x, y, z) as tocka) = 
    let _2D = [(x-1, y-1);(x-1, y);(x-1, y+1);(x, y-1);(x, y);(x, y+1);(x+1, y-1);(x+1, y);(x+1, y+1)] in
    let _3D = List.map (fun (i,j) -> [(i,j,z-1);(i,j,z);(i,j,z+1)]) _2D |> List.concat in
    List.filter (fun t -> t <> tocka) _3D

let okolica_4D ((x, y, z, w) as tocka) = 
    let _2D = [(x-1, y-1);(x-1, y);(x-1, y+1);(x, y-1);(x, y);(x, y+1);(x+1, y-1);(x+1, y);(x+1, y+1)] in
    let _3D = List.map (fun (i,j) -> [(i,j,z-1);(i,j,z);(i,j,z+1)]) _2D |> List.concat in
    let _4D = List.map (fun (i,j,k) -> [(i,j,k,w-1);(i,j,k,w);(i,j,k,w+1)]) _3D |> List.concat in
    List.filter (fun t -> t <> tocka) _4D

let poskus zacetno_stanje st_ciklov okolica = 
        let rec krog zive cikel = 
            if cikel = st_ciklov then List.length zive else
            let zivi_sosedi tocka = List.fold_left (
                fun acc tocka -> if List.mem tocka zive then acc+1 else acc) 0 (okolica tocka) in
            let drop_ponovitve sez = 
                let rec aux spomin = function
                    | [] -> spomin
                    | x::xs -> if (List.mem x spomin) then aux spomin xs else aux (x::spomin) xs
                in
                aux [] sez
            in
            let poglej_sosede sez = 
                let rec aux spomin = function
                    | [] -> spomin
                    | x::xs -> 
                        let st_sosedov = zivi_sosedi x in
                        if (zivi_sosedi x < 2) || (zivi_sosedi x > 3) then aux spomin xs 
                        else aux ((x, st_sosedov)::spomin) xs
                in
                aux [] sez
            in
            let kandidati = zive |> List.map (fun tocka -> okolica tocka) 
            |> (fun tocke -> zive::tocke) |> List.concat |> drop_ponovitve |> poglej_sosede in
            let rec aux nove = function
                | [] -> krog nove (cikel+1)
                | (kand, st_sosedov)::ostali ->
                    match List.mem kand zive with
                        | true -> aux (kand::nove) ostali
                        | false when st_sosedov = 3 -> aux (kand::nove) ostali
                        | _ -> aux nove ostali
            in
            aux [] kandidati
        in
        krog zacetno_stanje 0

let naloga1 vsebina_datoteke =
    let initial = vsebina_datoteke |> vsebina_v_podatke in
    let zive_zacetek tabela = 
        let zive = ref [] in
        for i = 0 to (Array.length tabela - 1) do
            for j = 0 to (Array.length tabela.(0) - 1) do
                if tabela.(i).(j) = '#' then zive := (i,j,0)::(!zive) else ()
            done;
        done;
        !zive
    in
    poskus (zive_zacetek initial) 6 okolica_3D |> string_of_int

let naloga2 vsebina_datoteke =
    let initial = vsebina_datoteke |> vsebina_v_podatke in
    let zive_zacetek tabela = 
        let zive = ref [] in
        for i = 0 to (Array.length tabela - 1) do
            for j = 0 to (Array.length tabela.(0) - 1) do
                if tabela.(i).(j) = '#' then zive := (i,j,0,0)::(!zive) else ()
            done;
        done;
        !zive
    in
    poskus (zive_zacetek initial) 6 okolica_4D |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_17.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_17_1.out" odgovor1;
    izpisi_datoteko "day_17_2.out" odgovor2