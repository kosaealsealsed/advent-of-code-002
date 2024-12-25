(* Read the grid from the input file *)
let read_grid file_path =
  let ic = open_in file_path in
  let rec read_lines acc =
    try
      let line = input_line ic |> String.trim in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

(* Check if a word exists in a given direction *)
let check_word grid x y dx dy word rows cols =
  let len = String.length word in
  let rec check i =
    if i = len then true
    else
      let nx = x + i * dx in
      let ny = y + i * dy in
      if nx < 0 || ny < 0 || nx >= rows || ny >= cols || String.get grid.(nx) ny <> String.get word i then
        false
      else
        check (i + 1)
  in
  check 0

(* Count occurrences of the word "XMAS" in all directions *)
let count_xmas grid =
  let target_word = "XMAS" in
  let directions = [ (0, 1); (1, 0); (1, 1); (1, -1); (0, -1); (-1, 0); (-1, -1); (-1, 1) ] in
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  let count = ref 0 in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      List.iter
        (fun (dx, dy) ->
          if check_word grid r c dx dy target_word rows cols then incr count)
        directions
    done
  done;
  !count

(* Count all X-MAS patterns in the grid *)
let count_all_xmas_patterns grid =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  let count = ref 0 in
  for r = 1 to rows - 2 do
    for c = 1 to cols - 2 do
      let center = String.get grid.(r) c in
      let top_left = String.get grid.(r - 1) (c - 1) in
      let top_right = String.get grid.(r - 1) (c + 1) in
      let bottom_left = String.get grid.(r + 1) (c - 1) in
      let bottom_right = String.get grid.(r + 1) (c + 1) in
      if center = 'A' then (
        (* Pattern 1: M.S *)
        if top_left = 'M' && top_right = 'S' && bottom_left = 'M' && bottom_right = 'S' then incr count;
        (* Pattern 2: S.M *)
        if top_left = 'S' && top_right = 'M' && bottom_left = 'S' && bottom_right = 'M' then incr count;
        (* Pattern 3: M.M *)
        if top_left = 'M' && top_right = 'M' && bottom_left = 'S' && bottom_right = 'S' then incr count;
        (* Pattern 4: S.S *)
        if top_left = 'S' && top_right = 'S' && bottom_left = 'M' && bottom_right = 'M' then incr count)
    done
  done;
  !count

(* Main function *)
let () =
  let file_path = "input.txt" in
  let grid = read_grid file_path |> Array.of_list in
  if Array.length grid = 0 then
    print_endline "Error: Grid is empty or invalid."
  else
    let xmas_count = count_xmas grid in
    Printf.printf "Count of XMAS: %d\n" xmas_count;
    let xmas_patterns = count_all_xmas_patterns grid in
    Printf.printf "Total X-MAS patterns: %d\n" xmas_patterns
