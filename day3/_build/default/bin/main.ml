(*Stores x,y coordinate of first digit of number*)
type part_number = {part_location_x: int; part_location_y: int; value: int}
type symbol = {sym_location_x: int; sym_location_y: int; char: char}

let rec read_lines () =
    let line = read_line () in
    if line = ""
    then []
    else line :: read_lines ()

let rec get_digit_count number =
    if (number/10) <> 0
    then (get_digit_count (number/10)) + 1
    else 1

let string_after_index string index =
    let rec creation_helper string current_index =
        if current_index >= String.length string
        then ""
        else String.make 1 (String.get string current_index) ^ creation_helper string (current_index+1)
    in
    creation_helper string index


(*Iterates over line, checking for part number at current index*)
let rec get_part_numbers_from_string_list line index =
    if index >= String.length line
    then
        [] 
    else
        let culled_line = string_after_index line index in
        try
            let current_value = Scanf.sscanf culled_line "%d" (fun x->x) in
            (*y location gets filled in later, number x is offset by 1 if negative to account for -*)
            let part_number = {part_location_x=index + if current_value < 0 then 1 else 0; part_location_y=0; value=Int.abs current_value} in
            part_number :: get_part_numbers_from_string_list line (index+(get_digit_count current_value)+if current_value<0 then 1 else 0) 
        with 
        | _ -> (
            try
                let periods = Scanf.sscanf culled_line "%[^0-9]" (fun s -> s) in
                get_part_numbers_from_string_list line (index + String.length periods)
            with
            | _ -> get_part_numbers_from_string_list line (index+1)
        )

let is_symbol = function '0'..'9' -> false | '.' -> false | _ -> true
let is_gear_symbol = function '*' -> true | _ -> false

let get_all_part_numbers_from_lines line_list =
    let rec iterate_over_all_lines lines y_index =
        if y_index >= List.length lines
        then []
        else
            let current_string_list = List.nth lines y_index in
            let retrieved_parts_from_line = get_part_numbers_from_string_list current_string_list 0 in
            (*y location of part filled in here*)
            let retrieved_parts_from_line = List.map (
                fun part ->  { part_location_x = part.part_location_x ; part_location_y = y_index ; value=part.value }
            ) retrieved_parts_from_line in
            retrieved_parts_from_line @ iterate_over_all_lines lines (y_index+1) in
    iterate_over_all_lines line_list 0

let get_symbol_locations_from_lines lines =
    let rec row_iterator lines index_y =
        let rec char_iterator line index_x =
            if index_x >= String.length line
            then []
            else
                let current_char = String.get line index_x in
                if is_symbol current_char
                then 
                    {sym_location_x=index_x; sym_location_y=index_y; char=current_char}
                    :: char_iterator line (index_x+1)
                else char_iterator line (index_x+1) in
        if index_y >= List.length lines
        then []
        else
            let current_line = List.nth lines index_y in
            char_iterator current_line 0 @ row_iterator lines (index_y+1)
    in
    row_iterator lines 0

let is_adjacent symbol part =
    if Int.abs (symbol.sym_location_y-part.part_location_y) <= 1
    then
        (*Checks if ANY digit of part number is <=1 away from symbol*)
        let rec is_adjacent_recursor symbol part offset =
            if offset >= (get_digit_count part.value )
            then false
            else 
                if Int.abs (symbol.sym_location_x - (part.part_location_x+offset)) <= 1
                then
                    let () = Printf.printf "Symbol char:%c x:%d y:%d adjacent to val:%d x:%d y:%d offset: %d\n"
                        symbol.char symbol.sym_location_x symbol.sym_location_y
                        part.value part.part_location_x part.part_location_y offset in
                    true
                else
                    is_adjacent_recursor symbol part (offset+1)
        in
        is_adjacent_recursor symbol part 0
    else false

let _sum_all_adjacent symbol_list part_list =
    let rec symbol_iterator index_sym =
        let rec part_iterator index_part =
            if index_part >= List.length part_list
            then 0
            else
                let current_symbol = List.nth symbol_list index_sym in
                let current_part = List.nth part_list index_part in
                if is_adjacent current_symbol current_part
                then current_part.value + part_iterator (index_part+1)
                else part_iterator (index_part+1)
        in
        if index_sym >= List.length symbol_list
        then 0
        else part_iterator 0 + symbol_iterator (index_sym+1)
    in
    symbol_iterator 0

let get_adjacent_to_gear_list symbol_list part_list =
    let rec symbol_iterator index_sym =
        let rec part_iterator index_part =
            if index_part >= List.length part_list
            then []
            else
                let current_symbol = List.nth symbol_list index_sym in
                let current_part = List.nth part_list index_part in
                if is_adjacent current_symbol current_part
                then current_part :: part_iterator (index_part+1)
                else part_iterator (index_part+1)
        in
        if index_sym >= List.length symbol_list
        then []
        else
            if is_gear_symbol (List.nth symbol_list index_sym).char
            then part_iterator 0 :: symbol_iterator (index_sym+1)
            else symbol_iterator (index_sym+1)
    in
    symbol_iterator 0

let sum_gears part_list_list =
    let rec list_iterator list_list index =
        if index >= List.length list_list
        then 0
        else
            let current_gear_pair = List.nth list_list index in
            if (List.length current_gear_pair) = 2
            then
                (List.nth current_gear_pair 0).value*(List.nth current_gear_pair 1).value + list_iterator list_list (index+1)
            else list_iterator list_list (index+1)
            in
    list_iterator part_list_list 0

let _print_part_number_info part_number = 
    Printf.printf "%d %d %d\n" part_number.part_location_x part_number.part_location_y part_number.value

let _print_symbol_location_info sym =
    Printf.printf "%d %d %c\n" sym.sym_location_x sym.sym_location_y sym.char

let () =
    let lines = read_lines () in
    let part_numbers = get_all_part_numbers_from_lines lines in
    let () = List.iter (fun part -> _print_part_number_info part) part_numbers in
    let symbol_locations = get_symbol_locations_from_lines lines in
    print_endline (string_of_int (sum_gears (get_adjacent_to_gear_list symbol_locations part_numbers)))

