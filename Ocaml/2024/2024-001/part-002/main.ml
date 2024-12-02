(* Read the input file and parse it into two lists *)
let read_file file_path =
  let left_list = ref [] in
  let right_list = ref [] in
  let ic = open_in file_path in
  try
    while true do
      let line = input_line ic in
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

(* Build a frequency dictionary for the right list *)
let build_frequency_map lst =
  List.fold_left (fun acc x ->
    let count = try Hashtbl.find acc x with Not_found -> 0 in
    Hashtbl.replace acc x (count + 1);
    acc
  ) (Hashtbl.create (List.length lst)) lst

(* Calculate the similarity score *)
let calculate_similarity_score left_list right_list =
  let freq_map = build_frequency_map right_list in
  List.fold_left (fun acc left ->
    let count = try Hashtbl.find freq_map left with Not_found -> 0 in
    acc + (left * count)
  ) 0 left_list

(* Main function *)
let () =
  let file_path = "input.txt" in
  let left_list, right_list = read_file file_path in
  let similarity_score = calculate_similarity_score left_list right_list in
  Printf.printf "Similarity Score: %d\n" similarity_score
