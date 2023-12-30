type 'a tree =
    | Leaf
    | Node of 'a node
    and 'a node = {
        value: 'a;
        left: 'a tree;
        right: 'a tree
}

let rec mem x = function
    | Leaf -> false
    | Node {value; left; right} -> value = x || mem x left || mem x right

let () =
    let t = Node {
        value = 2;
        left = Node { value = 1; left = Leaf; right = Leaf };
        right = Node {
            value = 4;
            left = Node { value = 3; left = Leaf; right = Leaf };
            right = Node { value = 7; left = Leaf; right = Leaf }
        }
    } in
    print_endline ( string_of_bool ( mem 8 t ) )
