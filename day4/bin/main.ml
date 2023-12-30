type card_type = {id: int; winning_numbers: int list; card_numbers: int list}

let rec read_lines () =
    let line = read_line () in
    match line with
    | "" -> []
    | _ -> line :: read_lines ()


let extract_numbers string =
    let strings = Str.split (Str.regexp " ") string in
    let strings = List.map (fun string -> String.trim string) strings |> List.filter (fun s -> s <> "") in
    let rec extract_numbers_rec strings =
        let () = Printf.printf "Entered extract_numbers_rec, length: %d\n" (List.length strings) in
        match strings with
        | [] -> let () = print_endline "Matched empty" in []
        | num :: tail -> 
            let () = Printf.printf "matched string %s\n" num in
            let extracted = Scanf.sscanf num "%d" (fun x -> x) in 
            extracted :: extract_numbers_rec tail
            in
    let () = print_endline "finished extracting" in
    extract_numbers_rec strings

let rec card_split lines =
    let () = print_endline "parsed lines!" in
    match lines with
    | [] -> [||]
    | head :: tail ->
            let () = Printf.printf "Parsing line: %s\n" head in
            let strings = Str.split (Str.regexp "[:|]") head in
            let () = Printf.printf "Parsed into %d separate strings!\n" (List.length strings) in
            match strings with
            | [a; b; c] ->
                    let game_id = Scanf.sscanf a "Card %d" (fun x -> x) in
                    let () = Printf.printf "Extracted game id %d\n" game_id in
                    let winning_numbers = extract_numbers (String.trim b) in
                    let card_numbers = extract_numbers (String.trim c) in
                    Array.append
                        ( [|[|{id = game_id; winning_numbers = winning_numbers; card_numbers = card_numbers}|]|] )
                        ( card_split tail )
            | _ -> [||]

let rec common_elements list_1 list_2 =
    match list_1 with
    | [] -> []
    | head::tail -> List.filter (fun x -> x = head) list_2 @ common_elements tail list_2

let _print_card_info card =
    let () = Printf.printf "card=%d : " card.id in
    let () = List.iter (fun number -> Printf.printf "%d " number ) card.winning_numbers in
    let () = Printf.printf " : " in
    let () = List.iter (fun number -> Printf.printf "%d " number ) card.card_numbers in 
    let () = Printf.printf " : " in
    let () = List.iter (fun number -> Printf.printf "%d " number ) (common_elements card.card_numbers card.winning_numbers) in
    print_endline ""

let rec pow base = function
    | 0 -> 1
    | 1 -> base
    | n -> 
            let b = pow base (n/2) in
            b*b*(if n mod 2 = 0 then 1 else base)

let card_value card =
    let common = common_elements card.winning_numbers card.card_numbers in
    match common with
    | [] -> 0
    | _ -> pow 2 ((List.length common) - 1)

let winning_no_count card =
    let common = common_elements card.winning_numbers card.card_numbers in
    List.length common


let rec sum_list list conv =
    match list with
    | [] -> 0
    | head::tail -> (conv head) + sum_list tail conv

let extend_array_copy_n arr n =
    Array.make (Array.length arr + n) arr.( 0 )

let win_cards card_arr_arr =
    Array.iteri (fun index card_arr -> 
        let win_count = winning_no_count (card_arr.(0)) in
        let no_of_cards = Array.length card_arr in
        let rec add_cards counter =
            if counter > win_count
            then ()
            else let () = Array.set card_arr_arr (index+counter) (extend_array_copy_n (Array.get card_arr_arr (index+counter) ) no_of_cards) in
                add_cards (counter+1) in
        add_cards 1
    ) card_arr_arr

let print_card_arr_arr arr_arr =
    Array.iter (
        fun card_arr -> (
            let () = Array.iter (fun card -> _print_card_info card) card_arr in
            print_endline ""
        )
    ) arr_arr


let sum_arr_arr_lengths arr_arr =
    let arr_list = Array.to_list arr_arr in
    let list_list = List.map (fun arr -> Array.to_list arr) arr_list in
    let rec sum_list_list_lengths list_list =
        match list_list with
        | [] -> 0
        | head::tail -> List.length head + sum_list_list_lengths tail in
    sum_list_list_lengths list_list
    

let () =
    let lines = read_lines () in
    let cards = card_split lines in
    let ()  = win_cards cards in
    print_endline (string_of_int (sum_arr_arr_lengths cards)) 

