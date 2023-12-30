type part_number = {location_x: int; location_y: int; value: int}

let rec read_lines () =
    let line = read_line () in
    if line = ""
    then []
    else line :: read_lines ()

let () =
    let lines = read_lines () in
    let concatenated = String.concat "::" lines in
    print_endline concatenated
