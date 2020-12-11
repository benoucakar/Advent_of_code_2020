let vsebina_v_podatke vsebina_datoteke = 
    let loci_crke niz = List.init (String.length niz) (fun i -> niz.[i]) in
    let sez_sez_chr = vsebina_datoteke |> String.trim |> String.split_on_char '\n' |> List.map loci_crke in
    Array.init (List.length sez_sez_chr) (fun i -> Array.of_list (List.nth sez_sez_chr i))

let naloga1 vsebina_datoteke =
    let prvotna_tabela = vsebina_v_podatke vsebina_datoteke in
    let dolzina = Array.length prvotna_tabela
    and sirina = Array.length prvotna_tabela.(0) in
    let okolica (i,j) podatki = 
        let kandidati = [(i-1,j-1);(i-1,j);(i-1,j+1);(i,j-1);(i,j+1);(i+1,j-1);(i+1,j);(i+1,j+1)]
        |> List.filter (fun (x,y) -> x >= 0 && x < dolzina && y >= 0 && y < sirina) in
        let rec prestej zased = function
            | [] -> zased
            | (x,y)::rep -> 
            match podatki.(x).(y) with
                | '#' -> prestej (zased+1) rep
                | _ -> prestej zased rep
        in prestej 0 kandidati
    in 
    let sprememba (i,j) podatki = 
        match podatki.(i).(j) with
            | 'L' when (okolica (i,j) podatki) = 0 -> '#'
            | '#' when (okolica (i,j) podatki) >= 4 -> 'L'
            | _ -> podatki.(i).(j)
    in
    let rec krog spomin = 
        let novo = Array.init dolzina (fun i -> Array.init sirina (fun j -> sprememba (i,j) spomin)) in
        if spomin = novo then
        novo |> Array.to_list |> Array.concat |> Array.fold_left (fun acc x -> if x = '#' then (acc+1) else acc) 0
        else krog novo
    in
    krog prvotna_tabela |> string_of_int

let naloga2 vsebina_datoteke =
    let prvotna_tabela = vsebina_v_podatke vsebina_datoteke in
    let dolzina = Array.length prvotna_tabela
    and sirina = Array.length prvotna_tabela.(0) in
    let okolica (i,j) podatki = 
        let rec najdi (x_korak, y_korak) (x_tr, y_tr)= 
            let (x_nv, y_nv) = (x_tr + x_korak, y_tr + y_korak) in
            if x_nv >= 0 && x_nv < dolzina && y_nv >= 0 && y_nv < sirina then
                if podatki.(x_nv).(y_nv) = '.' then najdi (x_korak, y_korak) (x_nv, y_nv)
                else podatki.(x_nv).(y_nv)
            else '.'
        in
        let premiki = [(1,1);(1,0);(1,-1);(0,1);(0,-1);(-1,1);(-1,0);(-1,-1)] in
        let rec prestej  zased = function
            | [] -> zased
            | koraki::rep -> 
            match najdi koraki (i,j) with
                | '#' -> prestej (zased+1) rep
                | _ -> prestej zased rep
        in prestej 0 premiki
    in 
    let sprememba (i,j) podatki = 
        match podatki.(i).(j) with
            | 'L' when (okolica (i,j) podatki) = 0 -> '#'
            | '#' when (okolica (i,j) podatki) >= 5 -> 'L'
            | _ -> podatki.(i).(j)
    in
    let rec krog spomin = 
        let novo = Array.init dolzina (fun i -> Array.init sirina (fun j -> sprememba (i,j) spomin)) in
        if spomin = novo then
        novo |> Array.to_list |> Array.concat |> Array.fold_left (fun acc x -> if x = '#' then (acc+1) else acc) 0
        else krog novo
    in
    krog prvotna_tabela |> string_of_int
    
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
    let vsebina_datoteke = preberi_datoteko "day_11.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_11_1.out" odgovor1;
    izpisi_datoteko "day_11_2.out" odgovor2