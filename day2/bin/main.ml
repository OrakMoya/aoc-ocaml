type _game = {id: int; red: int; green: int; blue: int}
type block_counts = {reds: int; greens: int; blues: int}

let rec _read_lines () =
    let line = read_line() in
    if line = ""
    then []
    else line::_read_lines ()

let strip str = 
    let str = Str.replace_first (Str.regexp "^ +") "" str in
    Str.replace_first (Str.regexp " +$") "" str

let parse_data_strings string_list =
    let rec count_red string_list index =
        if index >= List.length string_list
        then 0
        else
            let current_string = List.nth string_list index in
            let reds = try Scanf.sscanf current_string "%d red" (fun x -> x)
                with Scanf.Scan_failure(_string) -> 0 in
            let next = count_red string_list (index+1) in
            if next > reds
            then next
            else reds in
    let rec count_green string_list index =
        if index >= List.length string_list
        then 0
        else
            let current_string = List.nth string_list index in
            let greens = try Scanf.sscanf current_string "%d green" (fun x -> x)
                with Scanf.Scan_failure(_string) -> 0 in
            let next = count_green string_list (index+1) in
            if next > greens
            then next
            else greens in
    let rec count_blue string_list index =
        if index >= List.length string_list
        then 0
        else
            let current_string = List.nth string_list index in
            let blues = try Scanf.sscanf current_string "%d blue" (fun x -> x)
                with Scanf.Scan_failure(_string) -> 0 in
            let next = count_blue string_list (index+1) in
            if next > blues
            then next
            else blues in
    let reds = count_red string_list 0 in
    let greens = count_green string_list 0 in
    let blues = count_blue string_list 0 in
    {reds = reds; greens = greens; blues = blues}

let parse_string_as_game string =
    let split_id_from_rest = Str.split (Str.regexp ":") string in
    let game_id_string = List.nth split_id_from_rest 0 in
    let data_string = List.nth split_id_from_rest 1 in
    let _game_id = Scanf.sscanf game_id_string "Game %d" (fun x -> x) in
    let split_data = Str.split (Str.regexp "[,;]") data_string in
    let split_data = List.map (fun a -> strip a) split_data in
    let block_counts = parse_data_strings split_data in
    let () = Printf.printf "game: %d, %d %d %d\n" _game_id block_counts.reds block_counts.greens block_counts.blues in
    {id = _game_id; red = block_counts.reds; green = block_counts.greens; blue = block_counts.blues}


let _determine_impossible game reds greens blues =
    let () = Printf.printf "Checking game %d, %d %d %d" game.id game.red game.green game.blue in
    if game.red > reds || game.green > greens || game.blue > blues
    then let () = print_endline "\tpossible" in false
    else let () = print_endline "\timpossible" in true

let count_impossible_games game_list =
    let rec check game_list index =
        if index >= (List.length game_list)
        then 0
        else
            let current_game = List.nth game_list index in
            (current_game.red * current_game.green * current_game.blue) + (check game_list (index+1)) in
    check game_list 0

let () =
    let input_lines = _read_lines () in
    let games = List.map (fun string -> parse_string_as_game string) input_lines in
    let impossibles = count_impossible_games games in
    print_endline (string_of_int impossibles)

    
