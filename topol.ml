exception Cykliczne

let topol input_list =
    (* creates graph (map) from provided list *)
    let make l =
        let f acc (a, alist) =
            PMap.add a (alist, false, false) acc
        in
        List.fold_left f (PMap.create compare) l
    in
    let graph = ref (make input_list) in
    let w = ref [] in
    let rec dfs x =
        let fnd = PMap.find x !graph in
        match fnd with
        | (_, true, false) -> raise Cykliczne
        | (_, true, true) -> ()
        | (xlist, _, _) ->
        begin
            graph := PMap.add x (xlist, true, false) !graph;
            List.iter dfs xlist;
            graph := PMap.add x (xlist, true, true) !graph;
            w := x::!w;
        end
    in
    List.iter (fun (x, _) -> dfs x) input_list;
    !w
;;

(*
TESTY

topol [(1, [2;3]); (2, [3]); (3, [])];;
topol [(1, [2;3]); (2, [3]); (3, [1])];;
topol [(1, [2]); (2, [3;4]); (3, [5;6;7]); (4, []); (5, []); (6, [8]); (7, []); (8, [9]); (9, [10]); (10, [])];;
topol [(1, [2]); (2, [3;4]); (3, [5;6;7]); (4, [3]); (5, []); (6, [8]); (7, []); (8, [9]); (9, [10]); (10, [7])];;
*)