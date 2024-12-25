(* Read the input file and parse it into two lists *)
let read_file file_path =
  let left_list = ref [] in
  let right_list = ref [] in
  let ic = open_in file_path in
  try
    while true do
      let line = input_line ic in
      (* Trim leading and trailing whitespace *)
      let line = String.trim line in
      if line <> "" then
        match String.split_on_char ' ' line |> List.filter (fun s -> s <> "") with
        | [left; right] ->
            left_list := int_of_string left :: !left_list;
            right_list := int_of_string right :: !right_list
        | _ -> failwith "Malformed line in input file"
    done;
    (!left_list, !right_list)
  with End_of_file ->
    close_in ic;
    (!left_list, !right_list)

(* Function to calculate total distance *)
let calculate_total_distance left_list right_list =
  let sorted_left = List.sort compare left_list in
  let sorted_right = List.sort compare right_list in
  List.fold_left2 (fun acc left right -> acc + abs (left - right)) 0 sorted_left sorted_right

(* Main function *)
let () =
  let file_path = "input.txt" in
  let left_list, right_list = read_file file_path in
  let total_distance = calculate_total_distance left_list right_list in
  Printf.printf "Total Distance: %d\n" total_distance
