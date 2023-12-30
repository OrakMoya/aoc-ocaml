let rec read_lines_into_list () =
    let line = read_line() in
    if line = "" then []
    else line::read_lines_into_list ()

let explode string =
    let rec exp index list =
        if index < 0 then list else exp (index-1) (string.[index]::list) in
    exp (String.length string - 1) []

let is_digit = function '0' .. '9' -> true | _ -> false

let _string_digit_to_number = function
    | "zero"    ->  0
    | "one"     ->  1
    | "two"     ->  2
    | "three"   ->  3
    | "four"    ->  4
    | "five"    ->  5
    | "six"     ->  6
    | "seven"   ->  7
    | "eight"   ->  8
    | "nine"    ->  9
    | _ -> raise(Invalid_argument "Not a digit")

let _string_first_numerical_digit string =
    let char_list = explode string in
    let rec first_digit char_list index =
        if index > (List.length char_list - 1) then raise(Not_found)
        else let current_character = List.nth char_list index in
            if is_digit current_character then current_character
            else first_digit char_list (index+1) in
    first_digit char_list 0

let _string_first_numerical_digit_index string =
    let char_list = explode string in
    let rec first_digit char_list index =
        if index > (List.length char_list - 1) then raise(Not_found)
        else let current_character = List.nth char_list index in
            if is_digit current_character then index
            else first_digit char_list (index+1) in
    first_digit char_list 0

let _string_last_numerical_digit string =
    let char_list = explode string in
    let rec last_digit char_list index =
        if index < 0 then raise(Not_found)
        else let current_character = List.nth char_list index in
            if is_digit current_character then current_character
            else last_digit char_list (index-1) in
    last_digit char_list (List.length char_list - 1)

let _string_last_numerical_digit_index string =
    let char_list = explode string in
    let rec last_digit char_list index =
        if index < 0 then raise(Not_found)
        else let current_character = List.nth char_list index in
            if is_digit current_character then index
            else last_digit char_list (index-1) in
    last_digit char_list (List.length char_list - 1)


let int_of_char char =
    if is_digit char then Char.code char - Char.code '0'
    else raise(Invalid_argument (Printf.sprintf "\"%c\" is not a digit!" char) )

let sum number_list =
    let rec sum_helper number_list index =
        if index >= List.length number_list then 0
        else (List.nth number_list index) + (sum_helper number_list (index + 1) ) in
    sum_helper number_list 0

let contains s1 s2 start =
    let expression = Str.regexp s2 in
    try ignore (Str.search_forward expression s1 start); true
    with Not_found -> false

let _index_of_last_substring s1 s2 =
    if not (contains s1 s2 0)
    then raise(Not_found)
    else
        let expression = Str.regexp s2 in
        Str.search_backward expression s1 (String.length s1)

let _index_of_first_substring s1 s2 =
    if not (contains s1 s2 0)
    then raise(Not_found)
    else
        let expression = Str.regexp s2 in
        Str.search_forward expression s1 0

let _digits_as_string_list = ["zero";"one";"two";"three";"four";"five";"six";"seven";"eight";"nine"]

let _string_contains_any string list =
    let rec check_over_list string list index =
        if index > List.length list - 1
        then false
        else 
            if contains string (List.nth list index) 0
            then true
            else check_over_list string list (index+1) in
    check_over_list string list 0

let _first_string_digit string list = 
    if _string_contains_any string list
    then
        (* Iterates over string and looks for any digit that matches string starting at current index *)
        let rec iterate_over_string string list string_index = 
            let rec iterate_over_list string list string_index list_index =
                if list_index >= List.length list
                then ""
                else
                    let expression = Str.regexp_string (List.nth list list_index) in
                    if Str.string_match expression string string_index
                    then List.nth list list_index
                    else iterate_over_list string list string_index (list_index+1) in
            let result_of_list_iteration = iterate_over_list string list string_index 0 in
            if result_of_list_iteration <> ""
            then result_of_list_iteration
            else iterate_over_string string list (string_index+1) in
        iterate_over_string string list 0
    else raise(Not_found)

let _last_string_digit string list = 
    if _string_contains_any string list
    then
        (* Iterates over string and looks for any digit that matches string starting at current index *)
        let rec iterate_over_string string list string_index = 
            let rec iterate_over_list string list string_index list_index =
                if list_index >= List.length list
                then ""
                else
                    let expression = Str.regexp_string (List.nth list list_index) in
                    if Str.string_match expression string string_index
                    then List.nth list list_index
                    else iterate_over_list string list string_index (list_index+1) in
            let result_of_list_iteration = iterate_over_list string list string_index 0 in
            if result_of_list_iteration <> ""
            then result_of_list_iteration
            else iterate_over_string string list (string_index-1) in
        iterate_over_string string list (String.length string - 1)
    else raise(Not_found)

let _first_numerical_or_string_digit string =
    if (try _string_first_numerical_digit_index string with
    | Not_found -> Int.max_int) < try(_index_of_first_substring string (_first_string_digit string _digits_as_string_list)) with | Not_found -> Int.max_int
    then int_of_char (_string_first_numerical_digit string)
    else _string_digit_to_number (_first_string_digit string _digits_as_string_list)

let _last_numerical_or_string_digit string =
    if (try _string_last_numerical_digit_index string with
    | Not_found -> Int.min_int) > try(_index_of_last_substring string (_last_string_digit string _digits_as_string_list)) with | Not_found -> Int.min_int
    then int_of_char (_string_last_numerical_digit string)
    else _string_digit_to_number (_last_string_digit string _digits_as_string_list)

let () =
    let read_string_list = read_lines_into_list () in 
    let read_numbers = List.map (fun string -> (_first_numerical_or_string_digit string)*10+(_last_numerical_or_string_digit string) ) read_string_list in
    let sum_of_numbers = sum read_numbers in
    Printf.printf "%d\n" sum_of_numbers

