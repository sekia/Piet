type codel_chooser = Left | Right

type direction = North | East | South | West

type point = int * int

type edges =
  { north: point * point;
    east: point * point;
    south: point * point;
    west: point * point }

type block =
  { color: Picture.color;
    edges: edges;
    size: int }

module ColorBlocks =
  struct
    module Codels =
      Set.Make (
          struct
            type t = point
            let compare (a : t) b = compare a b
          end)

    module EdgesMemo =
      Map.Make (
          struct
            type t = Codels.t
            let compare = Codels.compare
          end)

    module Map =
      Map.Make (
          struct
            type t = point
            let compare (a : t) b = compare a b
          end)

    type t = block Map.t

    let find = Map.find

    let scan_row picture i =
      let row = picture.(i) in
      snd
      @@ Array.fold_left
           (fun (j, acc) color ->
            let codels, blocks = match acc with
              | [] -> ([], [])
              | (prev_color, _) :: _ as blocks when prev_color <> color ->
                 ([], blocks)
              | (_, codels) :: blocks -> (codels, blocks) in
            (j + 1, (color, (j, i) :: codels) :: blocks))
           (0, [])
           row

    let edges codels =
      assert (codels <> Codels.empty);
      let edge_coordinates fixed_coord variable_coord =
        let variable_coords fixed_value =
          Codels.fold
            (fun codel (curr_min, curr_max) ->
             let c = variable_coord codel in (min curr_min c, max curr_max c))
            (Codels.filter
               (fun codel -> (fixed_coord codel) = fixed_value)
               codels)
            (max_int, 0) in
        let fixed1, fixed2 =
          Codels.fold
            (fun codel (curr_min, curr_max) ->
             let c = fixed_coord codel in (min curr_min c, max curr_max c))
            codels
            (max_int, 0) in
        let variable11, variable12 = variable_coords fixed1 in
        let variable21, variable22 = variable_coords fixed2 in
        (fixed1, variable11, variable12), (fixed2, variable21, variable22) in
      let (west_x, west_y_right, west_y_left),
          (east_x, east_y_left, east_y_right) = edge_coordinates fst snd in
      let (north_y, north_x_left, north_x_right),
          (south_y, south_x_right, south_x_left) = edge_coordinates snd fst in
      { north = ((north_x_left, north_y), (north_x_right, north_y));
        east = ((east_x, east_y_left), (east_x, east_y_right));
        south = ((south_x_left, south_y), (south_x_right, south_y));
        west = ((west_x, west_y_left), (west_x, west_y_right)) }

    let of_picture picture =
      assert (Array.fold_left
                (fun acc color -> acc && color = Picture.Black)
                true
                picture.(0));
      let first_row_codels = Array.mapi (fun i _ -> (i, 0)) picture.(0) in
      let first_block =
        (Picture.Black,
         ref (Codels.of_list @@ Array.to_list first_row_codels)) in
      let blocks =
        ref (Array.fold_left
               (fun acc codel -> Map.add codel first_block acc)
               Map.empty
               first_row_codels) in
      let join_codels color codels =
        let codel_set = Codels.of_list codels in
        let codel_sets =
          List.map (fun (x, y) -> Map.find (x, y - 1) !blocks) codels
          |> List.filter (fun (block_color, _) -> color = block_color)
          |> List.rev_map snd
          |> List.sort_uniq (fun a b -> Codels.compare !b !a) in
        match codel_sets with
        | [] ->
           let block = (color, ref codel_set) in
           blocks := Codels.fold
                       (fun codel acc -> Map.add codel block acc)
                       codel_set
                       !blocks
        | merged :: codel_sets ->
           let updated = List.rev_map (fun codels -> !codels) codel_sets
                         |> List.fold_left Codels.union codel_set in
           merged := Codels.union updated !merged;
           let merged_block = (color, merged) in
           blocks := Codels.fold
                       (fun codel acc -> Map.add codel merged_block acc)
                       updated
                       !blocks in
      for i = 1 to (Array.length picture) - 1 do
        List.iter
          (fun (color, codels) -> join_codels color codels)
          (scan_row picture i)
      done;
      let edges_memo = ref EdgesMemo.empty in
      Map.mapi
        (fun codel (color, codels) ->
         let edges =
           try
             EdgesMemo.find !codels !edges_memo
           with
           | Not_found ->
              let edges = edges !codels in
              edges_memo := EdgesMemo.add !codels edges !edges_memo;
              edges in
         { color = color; edges = edges; size = Codels.cardinal !codels })
        !blocks
  end

module Stack =
  struct
    type t = int list

    let empty = []

    let dump = function
      | [] -> "[]"
      | stack ->
         "[ " ^ (String.concat ", " @@ List.map string_of_int stack) ^ " ]"

    let push stack x = x :: stack

    let pop = function
      | [] -> ([], None)
      | x :: stack -> (stack, Some x)

    let pop2 = function
      | x :: y :: stack -> (stack, Some (x, y))
      | stack -> (stack, None)

    let roll stack depth n =
      if depth <= 0 then stack
      else
        let rec take rest depth acc =
          if depth = 0 then Some (List.rev acc, rest)
          else
            match rest with
            | [] -> None
            | x :: rest -> take rest (depth - 1) (x :: acc) in
        match take stack depth [] with
        | None -> stack
        | Some (taken, rest) ->
           let n = n mod depth in
           let n = if n < 0 then n + depth else n in
           match take taken n [] with
           | None -> assert (false)
           | Some (latter, former) -> former @ latter @ rest
  end

module Trace =
  Set.Make (
      struct
        type t = direction * point
        let compare (a : t) b = compare a b
      end)

type t =
  { blocks: ColorBlocks.t;
    codel_chooser: codel_chooser;
    debug: bool;
    direction: direction;
    point: point;
    stack: Stack.t }

let create ?(debug=false) picture =
  { blocks = ColorBlocks.of_picture picture;
    codel_chooser = Left;
    debug = debug;
    direction = East;
    point = (1, 1);
    stack = Stack.empty }

let block_at evaluator point = ColorBlocks.find point evaluator.blocks

let codel_chooser_name = function
  | Left -> "Left"
  | Right -> "Right"

let direction_name = function
  | North -> "North"
  | East -> "East"
  | South -> "South"
  | West -> "West"

let point_description point blocks =
  let block = ColorBlocks.find point blocks in
  let x, y = point in
  Printf.sprintf "(%d, %d) = %s" x y (Picture.color_name block.color)

let evaluator_description evaluator =
  let color =
    let open Picture in
    let block = block_at evaluator evaluator.point in
    Picture.color_name block.color in
  let codel_chooser = codel_chooser_name evaluator.codel_chooser in
  let direction = direction_name evaluator.direction in
  let x, y = evaluator.point in
  Printf.sprintf
    "Point: (%d, %d) = %s, Codel Chooser: %s, Direction: %s, Stack: %s"
    x y color codel_chooser direction (Stack.dump evaluator.stack)

let forward evaluator (x, y) =
  match evaluator.direction with
  | North -> (x, y - 1)
  | East -> (x + 1, y)
  | South -> (x, y + 1)
  | West -> (x - 1, y)

let rotate_direction evaluator =
  let direction = match evaluator.direction with
    | North -> East
    | East -> South
    | South -> West
    | West -> North in
  if evaluator.debug then
    prerr_endline
    @@ "DP rotated: "
       ^ (direction_name evaluator.direction)
       ^ " -> "
       ^ (direction_name direction);
  { evaluator with direction = direction }

let toggle_codel_chooser evaluator =
  let codel_chooser = match evaluator.codel_chooser with
    | Left -> Right
    | Right -> Left in
  if evaluator.debug then
    prerr_endline
    @@ "CC toggled: "
       ^ (codel_chooser_name evaluator.codel_chooser)
       ^ " -> "
       ^ (codel_chooser_name codel_chooser);
  { evaluator with codel_chooser = codel_chooser }

module Command =
  struct
    let darkness =
      let open Picture in
      function
      | Black | White -> invalid_arg "darkness"
      | Color (Light, _) -> 0
      | Color (Normal, _) -> 1
      | Color (Dark, _) -> 2

    let hue =
      let open Picture in
      function
      | Black | White -> invalid_arg "hue"
      | Color (_, Red) -> 0
      | Color (_, Yellow) -> 1
      | Color (_, Green) -> 2
      | Color (_, Cyan) -> 3
      | Color (_, Blue) -> 4
      | Color (_, Magenta) -> 5

    let darkness_change prev_color color =
      let prev_darkness, darkness = darkness prev_color, darkness color in
      (darkness - prev_darkness + 3) mod 3

    let hue_change prev_color color =
      let prev_hue, hue = hue prev_color, hue color in
      (hue - prev_hue + 6) mod 6

    let nop _ evaluator = evaluator

    let push prev_evaluator evaluator =
      let prev_block = block_at prev_evaluator prev_evaluator.point in
      { evaluator with stack = Stack.push evaluator.stack prev_block.size }

    let pop _ evaluator =
      let stack, _ = Stack.pop evaluator.stack in
      { evaluator with stack = stack }

    let unary_command command =
      fun _ evaluator ->
      let stack, popped = Stack.pop evaluator.stack in
      match popped with
      | None -> evaluator
      | Some x -> command { evaluator with stack = stack } x

    let binary_command command =
      fun _ evaluator ->
      let stack, popped = Stack.pop2 evaluator.stack in
      match popped with
      | None -> evaluator
      | Some (x, y) -> command { evaluator with stack = stack } x y

    let binary_arithmetic op =
      binary_command
        (fun evaluator x y ->
         { evaluator with stack = Stack.push evaluator.stack (op y x) })

    let add = binary_arithmetic ( + )

    let subtract = binary_arithmetic ( - )

    let multiply = binary_arithmetic ( * )

    let divide prev_evaluator evaluator =
      try binary_arithmetic ( / ) prev_evaluator evaluator
      with
      | Division_by_zero -> evaluator

    let mod_ prev_evaluator evaluator =
      let mod_ x y =
        let mod_ = x mod y in
        if mod_ * y >= 0 then mod_ else mod_ + y in
      try binary_arithmetic mod_ prev_evaluator evaluator
      with
      | Division_by_zero -> evaluator

    let not =
      unary_command
        (fun evaluator x ->
         let not = if x = 0 then 1 else 0 in
         { evaluator with stack = Stack.push evaluator.stack not })

    let greater =
      let greater x y = if x > y then 1 else 0 in
      binary_arithmetic greater

    let pointer =
      unary_command
        (fun evaluator x ->
         let x = x mod 4 in
         let x = if x < 0 then x + 4 else x in
         let evaluator = ref evaluator in
         for i = 1 to x do
           evaluator := rotate_direction !evaluator
         done;
         !evaluator)

    let switch =
      unary_command
        (fun evaluator x ->
         if x mod 2 = 0 then evaluator
         else toggle_codel_chooser evaluator)

    let duplicate =
      unary_command
        (fun evaluator x ->
         let stack = Stack.push evaluator.stack x in
         let stack = Stack.push stack x in
         { evaluator with stack = stack })

    let roll =
      binary_command
        (fun evaluator x y ->
         { evaluator with stack = Stack.roll evaluator.stack y x })

    let in_char _ evaluator =
      let char = input_char stdin in
      { evaluator with stack = Stack.push evaluator.stack @@ Char.code char }

    let in_number _ evaluator =
      { evaluator with stack = Stack.push evaluator.stack @@ read_int () }

    let out_char =
      unary_command
        (fun evaluator x ->
         print_string @@ Text.char x;
         evaluator)

    let out_number =
      unary_command
        (fun evaluator x ->
         print_int x;
         evaluator)

    let process prev_evaluator evaluator =
      let block_color evaluator =
        let block = block_at evaluator evaluator.point in
        block.color in
      let prev_color, color =
        block_color prev_evaluator, block_color evaluator in
      let dispatched, op_name =
        if prev_color = Picture.White || color = Picture.White then nop, "nop"
        else
          match hue_change prev_color color, darkness_change prev_color color
          with
          | 0, 0 -> nop, "nop"
          | 0, 1 -> push, "push"
          | 0, 2 -> pop, "pop"
          | 1, 0 -> add, "add"
          | 1, 1 -> subtract, "substract"
          | 1, 2 -> multiply, "multiply"
          | 2, 0 -> divide, "divide"
          | 2, 1 -> mod_, "mod"
          | 2, 2 -> not, "not"
          | 3, 0 -> greater, "greater"
          | 3, 1 -> pointer, "pointer"
          | 3, 2 -> switch, "switch"
          | 4, 0 -> duplicate, "duplicate"
          | 4, 1 -> roll, "roll"
          | 4, 2 -> in_number, "in_number"
          | 5, 0 -> in_char, "in_char"
          | 5, 1 -> out_number, "out_number"
          | 5, 2 -> out_char, "out_char"
          | _ -> assert (false) in
      if evaluator.debug then prerr_endline @@ "OP dispatched: " ^ op_name;
      dispatched prev_evaluator evaluator
  end

let step evaluator =
  if evaluator.debug then prerr_endline @@ evaluator_description evaluator;
  let rec slide evaluator trace =
    let forward = forward evaluator evaluator.point in
    let forward_block = block_at evaluator forward in
    match forward_block.color with
    | Picture.White -> slide { evaluator with point = forward } trace
    | Picture.Black ->
       if Trace.mem (evaluator.direction, evaluator.point) trace then None
       else
         let trace = Trace.add (evaluator.direction, evaluator.point) trace in
         let evaluator = rotate_direction @@ toggle_codel_chooser evaluator in
         slide evaluator trace
    | _ -> Some { evaluator with point = forward } in
  let rec move evaluator retry =
    if retry = 0 then None
    else
      let selector = match evaluator.codel_chooser with
        | Left -> fst
        | Right -> snd in
      let block = block_at evaluator evaluator.point in
      let edge_codel =
        let edge = match evaluator.direction with
          | North -> block.edges.north
          | East -> block.edges.east
          | South -> block.edges.south
          | West -> block.edges.west in
        selector edge in
      let forward = forward evaluator edge_codel in
      let forward_block = block_at evaluator forward in
      match forward_block.color with
      | Picture.Black ->
         let change_route =
           if retry mod 2 = 0 then toggle_codel_chooser
           else rotate_direction in
         move (change_route evaluator) (retry - 1)
      | _ -> Some { evaluator with point = forward } in
  let block = block_at evaluator evaluator.point in
  let next_evaluator =
    if block.color = Picture.White then slide evaluator Trace.empty
    else move evaluator 8 in
  match next_evaluator with
  | None -> None
  | Some next_evaluator ->
     if evaluator.debug then
       begin
         prerr_endline
         @@ "Moved to: "
            ^ (point_description next_evaluator.point next_evaluator.blocks);
         flush stderr
       end;
     Some (Command.process evaluator next_evaluator)

let run ?(debug=false) picture =
  let evaluator = create ~debug picture in
  let rec loop = function
    | None -> ()
    | Some evaluator -> loop @@ step evaluator in
  loop (Some evaluator)
