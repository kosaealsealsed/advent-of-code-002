open Printf
open Hashtbl

(* Read the file and split into rules and updates *)
let read_file file_path =
  let ic = open_in file_path in
  let rec read_lines acc =
    try
      let line = input_line ic in
      line :: acc |> read_lines
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  let content = read_lines [] in
  let rec split_sections acc rules_done = function
    | [] -> List.rev acc
    | "" :: rest -> split_sections acc true rest
    | line :: rest ->
      if rules_done then
        split_sections ((`Update line) :: acc) true rest
      else
        split_sections ((`Rule line) :: acc) false rest
  in
  split_sections [] false content


let parse_rules_and_updates content =
  let parse_rule line =
    let parts = String.split_on_char '|' line in
    match parts with
    | [x; y] -> (int_of_string x, int_of_string y)
    | _ -> failwith "Invalid rule format"
  in
  let parse_update line =
    line |> String.split_on_char ',' |> List.map int_of_string
  in
  List.fold_left
    (fun (rules, updates) -> function
      | `Rule line -> (parse_rule line :: rules, updates)
      | `Update line -> (rules, parse_update line :: updates))
    ([], [])
    content

(* Check if an update follows the rules *)
let is_update_ordered update rules =
  let index_map = Hashtbl.create (List.length update) in
  List.iteri (fun i page -> Hashtbl.add index_map page i) update;
  List.for_all
    (fun (x, y) ->
      match (Hashtbl.find_opt index_map x, Hashtbl.find_opt index_map y) with
      | Some xi, Some yi -> xi <= yi
      | _ -> true)
    rules

(* Perform topological sort on an update *)
let topological_sort_update update rules =
  let graph = Hashtbl.create (List.length update) in
  let in_degree = Hashtbl.create (List.length update) in
  let nodes = List.fold_left (fun acc node -> node :: acc) [] update in
  (* Build the graph and in-degree table *)
  List.iter
    (fun (x, y) ->
      if List.mem x nodes && List.mem y nodes then (
        let neighbors = Hashtbl.find_opt graph x |> Option.value ~default:[] in
        Hashtbl.replace graph x (y :: neighbors);
        Hashtbl.replace in_degree y (1 + (Hashtbl.find_opt in_degree y |> Option.value ~default:0)));
      if not (Hashtbl.mem in_degree x) then Hashtbl.add in_degree x 0)
    rules;
  (* Perform topological sort *)
  let queue = Queue.create () in
  List.iter
    (fun node ->
      if Hashtbl.find_opt in_degree node |> Option.value ~default:0 = 0 then
        Queue.add node queue)
    nodes;
  let sorted_update = ref [] in
  while not (Queue.is_empty queue) do
    let current = Queue.pop queue in
    sorted_update := current :: !sorted_update;
    let neighbors = Hashtbl.find_opt graph current |> Option.value ~default:[] in
    List.iter
      (fun neighbor ->
        Hashtbl.replace in_degree neighbor ((Hashtbl.find in_degree neighbor) - 1);
        if Hashtbl.find in_degree neighbor = 0 then Queue.add neighbor queue)
      neighbors
  done;
  List.rev !sorted_update

(* Calculate the middle page of a list *)
let middle_page lst =
  let len = List.length lst in
  List.nth lst (len / 2)

(* Main logic *)
let process_updates rules updates =
  let correct_updates, middle_pages =
    List.fold_left
      (fun (correct, mids) update ->
        if is_update_ordered update rules then
          (update :: correct, (middle_page update) :: mids)
        else
          (correct, mids))
      ([], [])
      updates
  in
  let sum_middle_pages = List.fold_left (+) 0 middle_pages in

  let incorrect_updates, incorrect_middle_pages =
    List.fold_left
      (fun (incorrect, mids) update ->
        if not (is_update_ordered update rules) then
          let corrected = topological_sort_update update rules in
          (corrected :: incorrect, (middle_page corrected) :: mids)
        else
          (incorrect, mids))
      ([], [])
      updates
  in
  let sum_incorrect_middle_pages = List.fold_left (+) 0 incorrect_middle_pages in

  (sum_middle_pages, sum_incorrect_middle_pages)

(* File path and execution *)
let file_path = "input.txt"

let () =
  let content = read_file file_path in
  let rules, updates = parse_rules_and_updates content in
  let sum_correct, sum_incorrect = process_updates rules updates in
  printf "Sum of middle pages (correct updates): %d\n" sum_correct;
  printf "Sum of middle pages (corrected updates): %d\n" sum_incorrect

