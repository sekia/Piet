type brightness = Light | Normal | Dark

type regular_color = Red | Yellow | Green | Cyan | Blue | Magenta

type color = Black | White | Color of brightness * regular_color

let color_name = function
  | Black -> "Black"
  | White -> "White"
  | Color (brightness, color) ->
     let brightness = match brightness with
       | Light -> "Light "
       | Normal -> ""
       | Dark -> "Dark " in
     let color = match color with
       | Red -> "Red"
       | Yellow -> "Yellow"
       | Green -> "Green"
       | Cyan -> "Cyan"
       | Blue -> "Blue"
       | Magenta -> "Magenta" in
     brightness ^ color

module ColorTable =
  Map.Make(
      struct
        type t = Color.rgb
        let compare = compare
      end)

let default_color_table =
  List.fold_left
    (fun acc (rgb, color) -> ColorTable.add rgb color acc)
    ColorTable.empty
    [
      ({ Color.r = 255; g = 192; b = 192 }, Color (Light, Red));
      ({ Color.r = 255; g = 255; b = 192 }, Color (Light, Yellow));
      ({ Color.r = 192; g = 255; b = 192 }, Color (Light, Green));
      ({ Color.r = 192; g = 255; b = 255 }, Color (Light, Cyan));
      ({ Color.r = 192; g = 192; b = 255 }, Color (Light, Blue));
      ({ Color.r = 255; g = 192; b = 255 }, Color (Light, Magenta));
      ({ Color.r = 255; g = 0; b = 0 }, Color (Normal, Red));
      ({ Color.r = 255; g = 255; b = 0 }, Color (Normal, Yellow));
      ({ Color.r = 0; g = 255; b = 0 }, Color (Normal, Green));
      ({ Color.r = 0; g = 255; b = 255 }, Color (Normal, Cyan));
      ({ Color.r = 0; g = 0; b = 255 }, Color (Normal, Blue));
      ({ Color.r = 255; g = 0; b = 255 }, Color (Normal, Magenta));
      ({ Color.r = 192; g = 0; b = 0 }, Color (Dark, Red));
      ({ Color.r = 192; g = 192; b = 0 }, Color (Dark, Yellow));
      ({ Color.r = 0; g = 192; b = 0 }, Color (Dark, Green));
      ({ Color.r = 0; g = 192; b = 192 }, Color (Dark, Cyan));
      ({ Color.r = 0; g = 0; b = 192 }, Color (Dark, Blue));
      ({ Color.r = 192; g = 0; b = 192 }, Color (Dark, Magenta));
      ({ Color.r = 0; g = 0; b = 0 }, Black);
      ({ Color.r = 255; g = 255; b = 255 }, White)
    ]

type t = color array array

let load_file
      ?(codel_size=1)
      ?(color_table=default_color_table)
      ?(unknown_color=Black)
      filename =
  let image = Images.load filename [ Images.Load_only_the_first_frame ] in
  let width, height = Images.size image in
  if width mod codel_size <> 0 || height mod codel_size <> 0 then
    invalid_arg
    @@ Printf.sprintf
         "load_file: image size is %dx%d but codel size is %d so \
          it cannot fill the image."
         width height codel_size;
  let rgb_image = match image with
    | Images.Index8 image -> Index8.to_rgb24 image
    | Images.Index16 image -> Index16.to_rgb24 image
    | Images.Rgb24 image -> image
    | Images.Rgba32 image -> Rgb24.of_rgba32 image
    | _ ->
       failwith "load_file: the image's color profile is not supported." in
  let picture_width, picture_height =
    width / codel_size, height / codel_size in
  let picture =
    Array.make_matrix (picture_height + 2) (picture_width + 2) Black in
  let sample_representive_pixel x y = x * codel_size, y * codel_size in
  for i = 1 to picture_height do
    for j = 1 to picture_width do
      picture.(i).(j) <-
        try
          let x, y = sample_representive_pixel (j - 1) (i - 1) in
          ColorTable.find (Rgb24.get rgb_image x y) color_table
        with
        | Not_found -> unknown_color
    done
  done;
  Images.destroy image;
  Rgb24.destroy rgb_image;
  picture
