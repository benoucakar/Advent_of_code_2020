let vsebina_v_podatke vsebina_datoteke = 
    let vrstice = vsebina_datoteke |> String.trim |> String.split_on_char '\n' in
    let omejitve niz =
        let naslov = niz |>  String.split_on_char ':' |> List.hd in
        let stevila = niz |> String.split_on_char ':' |> (fun l -> List.nth l 1) |> String.split_on_char ' ' 
        |> List.filter (fun niz -> String.contains niz '-') |> List.map (String.split_on_char '-') 
        |> List.concat |> List.map int_of_string in
        (naslov, List.nth stevila 0, List.nth stevila 1, List.nth stevila 2, List.nth stevila 3)
    in
    let stevila niz = niz |> String.split_on_char ',' |> List.map int_of_string in
    let rec precisti meje jaz drugi zastava = function
        | [] -> (meje, jaz |> Array.of_list, drugi)
        | x::xs -> 
        if x = "" then precisti meje jaz drugi (zastava + 1) (List.tl xs) else
        match zastava with
            | 0 -> precisti ((omejitve x)::meje) jaz drugi zastava xs
            | 1 -> precisti meje (stevila x) drugi zastava xs
            | _ -> precisti meje jaz ((stevila x)::drugi) zastava xs
    in
    precisti [] [] [] 0 vrstice

let naloga1 vsebina_datoteke =
    let (meje, _, ostali) = vsebina_datoteke |> vsebina_v_podatke in
    let vsa_stevila = List.concat ostali in
    let slabi = List.filter (fun n -> List.for_all (fun (_, x, y, z, w) -> n < x || (y < n && n < z) || w < n ) meje) vsa_stevila in
    List.fold_left (+) 0 slabi |> string_of_int

let naloga2 vsebina_datoteke =
    let (meje, jaz, ostali) = vsebina_datoteke |> vsebina_v_podatke in
    let zanima_nas = ["departure location"; "departure station"; "departure platform"; 
    "departure track"; "departure date"; "departure time"] in
    let dobre_vozovnice = List.filter (fun vozovnica -> List.for_all (
        fun n -> List.exists (fun (_, x, y, z, w) -> (x <= n && n <= y) || (z <= n && n <= w)) meje) vozovnica) ostali in
    let vozovnica_v_array_kand voz = 
        let st_v_kand n =
            let rec aux acc = function
                | [] -> acc
                | (ime, x, y, z, w)::rep -> if (x <= n && n <= y) || (z <= n && n <= w) then aux (ime::acc) rep else aux acc rep
            in
            aux [] meje
        in
        List.map st_v_kand voz |> Array.of_list
    in
    let presek_sez = function (* a' sez sez -> a' sez *)
        | [] -> []
        | prvi::ostali -> 
            let rec aux acc = function
                | [] -> acc
                | x::xs -> if List.for_all (List.mem x) ostali then aux (x::acc) xs else aux acc xs
            in
            aux [] prvi
    in
    let tabela_kandidatov = dobre_vozovnice |> List.map vozovnica_v_array_kand |> Array.of_list in
    let sirina_tabele = Array.length tabela_kandidatov.(0)
    and dolzina_tabele = Array.length tabela_kandidatov in
    let slovar_prekrivanje = ref [] in
    for j = 0 to (sirina_tabele - 1) do
        let stolpec = ref [] in
        for i = 0 to (dolzina_tabele - 1) do
            stolpec := (tabela_kandidatov.(i).(j))::(!stolpec);
        done;
        slovar_prekrivanje := (j, presek_sez !stolpec)::(!slovar_prekrivanje);
    done;
    slovar_prekrivanje := List.sort (fun (_, l) (_, l') -> (List.length l) - (List.length l')) !slovar_prekrivanje;
    let slovar = Array.make sirina_tabele "" in
    let rec pocisti spomin = function
        | [] -> ()
        | (i, sez)::xs ->
        let cist_sez = List.filter (fun x -> not (List.mem x spomin)) sez in
        slovar.(i) <- (List.hd cist_sez);
        pocisti ((List.hd cist_sez)::spomin) xs
    in
    pocisti [] (!slovar_prekrivanje);
    let resitev = ref 1 in
    for k = 0 to sirina_tabele - 1 do
        if List.mem slovar.(k) zanima_nas
        then resitev := !resitev * jaz.(k)
        else ()
    done;
    !resitev |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_16.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_16_1.out" odgovor1;
    izpisi_datoteko "day_16_2.out" odgovor2