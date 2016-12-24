open Extensions.Pervasives ;; 

(*************** Types ***************)

exception Wld_version_unsupported of string ;;
let supported_wld_version = 156;;

type header_data = 
  | Bool of bool | Byte of int
  | Int16 of int | Int32 of int32 | Int64 of int64
  | Single of float | Double of float
  | Array of header_data array
  | String of string
  | Unknown ;;

let int_of_header_data = function 
  | Int16 i -> i
  | Int32 i32 -> Int32.to_int i32
  | Int64 i64 -> Int64.to_int i64
  | Double f | Single f -> int_of_float f
  | _ -> raise @@ Invalid_argument "Can't convert type in int_of_data_type" ;;

(* Assoc list of header values *)
type header = (string * header_data) list ;;

type x = int ;;
type y = int ;;
type wall_type = int ;;
type tile_solid = int ;;
type tile_liquid = int ;;
type tile_type = NoType | Solid of tile_solid | Liquid of tile_liquid ;;
type tile_wall = NoWall | Wall of wall_type ;;
type tile_frame = NoFrame | Frame of (x * y) ;; 
type tile_wire = NoWire | Wire1 | Wire2 | Wire3 | Wire4
type tile_slope = NoSlope | HalfBrick | Slope of int ;;
type tile_color = NoColor | Color of int ;;

type tile_background = 
  { wire : tile_wire ;
    wall : tile_wall ;
    color : tile_color ;
    actuator : bool ;
    inactive : bool } ;;

type tile_foreground =
  { t : tile_type ;
    color : tile_color ;
    frame : tile_frame ;
    slope : tile_slope } ;;

type tile_data = 
  { front : tile_foreground ;
    back : tile_background }

type tile = 
    Tile of tile_data
  | EmptyTile ;;

type tiles = tile array array ;;

type position = 
  { x : int ;
    y : int } ;;


type item_data = 
 { id : int32 ;
   prefix : int ;
   stack : int } ;;

type item = NoItem | Item of item_data ;;

type chest = 
  { pos : position ;
    name : string ;
    items : item list } ;;

type sign = 
  { pos : position ;
    text : string } ;;

type entity = 
  { pos: position ;
    id: int; } ;;

type npc = 
  { active : bool ;
    name : string ;
    displayName : string ;
    positionX : string ;
    positionY : string ;
    homeless : bool ;
    homeTileX : int ;
    homeTileY : int } ;;

type pressure_plate = { pos: position } ;;

type world = 
  { header: header ;
    tiles: tiles ;
    chests: chest array ;
    signs: sign array ;
    npcs : npc array ;
    entities: entity array ;
    pressure_plates: pressure_plate array } ;;


(*************** Utilities ***************)

let int_of_boolbits arr =
  let sum tosum = Array.fold_left (fun x1 x2 -> x1+x2) 0 tosum in
  sum @@ Array.mapi (fun i x -> if x then (2 ^^ i) else 0) arr ;;


let byte_to_boolbits (byte : int) =
  let (|.|) data bit = data land bit == bit in
  [| byte |.| 1 ;
     byte |.| 2 ;
     byte |.| 4 ;
     byte |.| 8 ;
     byte |.| 16 ;
     byte |.| 32 ;
     byte |.| 64 ;
     byte |.| 128 |] ;;

let rec string_of_header_data = function
    | Bool b -> string_of_bool b
    | Byte i | Int16 i -> string_of_int i
    | Int32 i32 -> Int32.to_string i32
    | Single f | Double f -> string_of_float f
    | Int64 i64 -> Int64.to_string i64
    | String s -> s
    | Array a -> String.concat "," @@ List.map (string_of_header_data) (Array.to_list a)
    | Unknown -> "?????"

let string_of_header itm = 
  Printf.sprintf "%s: %s" (fst itm) (string_of_header_data (snd itm)) ;;

let print_header h = 
  List.iter (fun s -> print_endline @@ string_of_header s) h


(*************** World IO ***************)

(****** World Header ******)

let read_header inch = 
  let read = function
    | `Bool -> Bool (read_bool inch)
    | `Byte -> Byte (input_byte inch)
    | `Int16 -> Int16 (read_i16 inch)
    | `Int32 -> Int32 (read_i32 inch)
    | `Single -> Single (read_single inch)
    | `Double -> Double (read_double inch)
    | `Int64 -> Int64 (read_i64 inch)
    | `String -> String (read_pascal_string inch)
    | `ArrayString n -> Array (Array.init n (fun i -> String (read_pascal_string inch)))
    | `ArrayInt32 n -> Array (Array.init n (fun i -> Int32 (read_i32 inch)))
    | `ArrayBitBool n -> begin (* where n is # of bytes, not bits *)
        let read_boolbits _ = byte_to_boolbits (input_byte inch) in
        let to_bool = function b -> Bool b in
        Array (Array.map to_bool @@ Array.concat @@ Array.to_list @@ Array.init n read_boolbits)

      end
    | _ -> raise @@ Invalid_argument "Unknown type passed to read"
  in

  let a = Array.make 122 ("",(Unknown)) in
  a.(0) <- ("version", read `Int32); 
  a.(1) <- ("relogic", read `Int64);
  a.(2) <- ("revision", read `Int32);
  a.(3) <- ("favorite", read `Int64);
  a.(4) <- ("_num_position", read `Int16);
  a.(5) <- ("positions", read (`ArrayInt32 10));
  a.(6) <- ("_num_importance", read `Int16);
  a.(7) <- ("importance", read (`ArrayBitBool 58));
  a.(8) <- ("world_name", read `String);
  a.(9) <- ("world_id", read `Int32);
  a.(10) <- ("left_world_boundary", read `Int32);
  a.(11) <- ("right_world_boundary", read `Int32);
  a.(12) <- ("top_world_boundary", read `Int32);
  a.(13) <- ("bottom_world_boundary", read `Int32);
  a.(14) <- ("max_tiles_y", read `Int32);
  a.(15) <- ("max_tiles_x", read `Int32);
  a.(16) <- ("expert_mode", read `Bool);
  a.(17) <- ("creation_time", read `Double);
  a.(18) <- ("moon_type", read `Byte);
  a.(19) <- ("tree_x", read (`ArrayInt32 3)); (* size = 3 *)
  a.(20) <- ("tree_style", read (`ArrayInt32 4)); (* size = 4 *)
  a.(21) <- ("cave_back_x", read (`ArrayInt32 3)); (* size = 3 *)
  a.(22) <- ("cave_back_style", read (`ArrayInt32 4)); (* size = 4 *)
  a.(23) <- ("ice_back_style", read `Int32);
  a.(24) <- ("jungle_back_style", read `Int32);
  a.(25) <- ("hell_back_style", read `Int32);
  a.(26) <- ("spawn_tile_x", read `Int32);
  a.(27) <- ("spawn_tile_y", read `Int32);
  a.(28) <- ("world_surface", read `Double);
  a.(29) <- ("rock_layer", read `Double);
  a.(30) <- ("temp_time", read `Double);
  a.(31) <- ("temp_day_time", read `Bool);
  a.(32) <- ("temp_moon_phase", read `Int32);
  a.(33) <- ("temp_blood_moon", read `Bool);
  a.(34) <- ("temp_eclipse", read `Bool);
  a.(35) <- ("dungeon_x", read `Int32);
  a.(36) <- ("dungeon_y", read `Int32);
  a.(37) <- ("crimson", read `Bool);
  a.(38) <- ("downed_boss_1", read `Bool);
  a.(39) <- ("downed_boss_2", read `Bool);
  a.(40) <- ("downed_boss_3", read `Bool);
  a.(41) <- ("downed_queen_bee", read `Bool);
  a.(42) <- ("downed_mech_boss_1", read `Bool);
  a.(43) <- ("downed_mech_boss_2", read `Bool);
  a.(44) <- ("downed_mech_boss_3", read `Bool);
  a.(45) <- ("downed_mech_boss_any", read `Bool);
  a.(46) <- ("downed_plant_boss", read `Bool);
  a.(47) <- ("downed_golem_boss", read `Bool);
  a.(48) <- ("downed_slime_king", read `Bool);
  a.(49) <- ("saved_goblin", read `Bool);
  a.(50) <- ("saved_wizard", read `Bool);
  a.(51) <- ("saved_mech", read `Bool);
  a.(52) <- ("downed_goblins", read `Bool);
  a.(53) <- ("downed_clown", read `Bool);
  a.(54) <- ("downed_frost", read `Bool);
  a.(55) <- ("downed_pirates", read `Bool);
  a.(56) <- ("shadow_orb_smashed", read `Bool);
  a.(57) <- ("spawn_meteor", read `Bool);
  a.(58) <- ("shadow_orb_count", read `Byte);
  a.(59) <- ("altar_count", read `Int32);
  a.(60) <- ("hard_mode", read `Bool);
  a.(61) <- ("invasion_delay", read `Int32);
  a.(62) <- ("invasion_size", read `Int32);
  a.(63) <- ("invasion_type", read `Int32);
  a.(64) <- ("invasion_x", read `Double);
  a.(65) <- ("slime_rain_time", read `Double);
  a.(66) <- ("sundial_cooldown", read `Byte);
  a.(67) <- ("temp_raining", read `Bool);
  a.(68) <- ("temp_rain_time", read `Int32);
  a.(69) <- ("temp_max_rain", read `Single);
  a.(70) <- ("ore_tier1", read `Int32);
  a.(71) <- ("ore_tier2", read `Int32);
  a.(72) <- ("ore_tier3", read `Int32);
  a.(73) <- ("tree_bg", read `Byte);
  a.(74) <- ("corrupt_bg", read `Byte);
  a.(75) <- ("jungle_bg", read `Byte);
  a.(76) <- ("snow_bg", read `Byte);
  a.(77) <- ("hallow_bg", read `Byte);
  a.(78) <- ("crimson_bg", read `Byte);
  a.(79) <- ("desert_bg", read `Byte);
  a.(80) <- ("ocean_bg", read `Byte);
  a.(81) <- ("cloud_bg_active", read `Int32);
  a.(82) <- ("num_clouds", read `Int16);
  a.(83) <- ("wind_speed", read `Single);
  a.(84) <- ("_num_angler_finished", read `Int32);
  a.(85) <- ("angler_who_finished_today", read (`ArrayString (int_of_header_data (snd a.(84)))));
  a.(86) <- ("saved_angler", read `Bool);
  a.(87) <- ("angler_quest", read `Int32);
  a.(88) <- ("saved_stylist", read `Bool);
  a.(89) <- ("saved_tax_collector", read `Bool);
  a.(90) <- ("invasion_size_start", read `Int32);
  a.(91) <- ("temp_cultist_delay", read `Int32);
  a.(92) <- ("_num_npc_killed", read `Int16);
  a.(93) <- ("npc_kill_count", read (`ArrayInt32 (int_of_header_data (snd a.(92)))));
  a.(94) <- ("fast_forward_time", read `Bool);
  a.(95) <- ("downed_fishron", read `Bool);
  a.(96) <- ("downed_martians", read `Bool);
  a.(97) <- ("downed_ancient_cultist", read `Bool);
  a.(98) <- ("downed_moonlord", read `Bool);
  a.(99) <- ("downed_halloween_king", read `Bool);
  a.(100) <- ("downed_halloween_tree", read `Bool);
  a.(101) <- ("downed_christmas_ice_queen", read `Bool);
  a.(102) <- ("downed_christmas_santank", read `Bool);
  a.(103) <- ("downed_christmas_tree", read `Bool);
  a.(104) <- ("downed_tower_solar", read `Bool);
  a.(105) <- ("downed_tower_vortex", read `Bool);
  a.(106) <- ("downed_tower_nebula", read `Bool);
  a.(107) <- ("downed_tower_stardust", read `Bool);
  a.(108) <- ("tower_active_solar", read `Bool);
  a.(109) <- ("tower_active_vortex", read `Bool);
  a.(110) <- ("tower_active_nebula", read `Bool);
  a.(111) <- ("tower_active_stardust", read `Bool);
  a.(112) <- ("lunar_apocalypse_is_up", read `Bool);
  (* v 170 *)
  a.(113) <- ("temp_party_manual", read `Bool);
  a.(114) <- ("temp_party_genuine", read `Bool);
  a.(115) <- ("temp_party_cooldown", read `Int32);
  a.(116) <- ("_num_celebrating", read `Int32);
  a.(117) <- ("temp_party_celebrating_npcs", read (`ArrayInt32 (int_of_header_data @@ snd a.(116))));
  (* v 173 *)
  a.(118) <- ("temp_sandstorm_happening", read `Bool);
  a.(119) <- ("temp_sandstorm_time_left", read `Int32);
  a.(120) <- ("temp_sandstorm_severity", read `Single);
  a.(121) <- ("temp_sandstorm_intended_severity", read `Single);

  (* Return an assoc list from array
   * This provides convenient header access through List.assoc *)
  Array.to_list a
;;


(****** World Tiles ******)

let read_tile_flags in_ch = 
  let flags3 = byte_to_boolbits @@ input_byte in_ch in
  let flags2 = byte_to_boolbits @@ if (flags3.(0)) then input_byte in_ch else 0 in
  let flags1 = byte_to_boolbits @@ if (flags2.(0)) then input_byte in_ch else 0 in
  (flags3,flags2,flags1) ;;

type tile_read = 
  { t : tile ;
    k : int } ;;

let wire_to_string = function
  | NoWire -> "NoWire"
  | Wire1 -> "Wire1"
  | Wire2 -> "Wire2"
  | Wire3 -> "Wire3"
  | Wire4 -> "Wire4" ;;

let wall_to_string = function
  | NoWall -> "NoWall"
  | Wall n -> Printf.sprintf "Wall (%d)" n ;;

let color_to_string = function
  | NoColor -> "NoColor"
  | Color n -> Printf.sprintf "Color (%d)" n ;;


let tile_t_to_string = function
  | NoType -> "NoType"
  | Solid n ->  Printf.sprintf "Solid (%d)" n
  | Liquid l ->  Printf.sprintf "Liquid (%d)" l ;;

let tile_tt_to_string = function
  | Tile t -> tile_t_to_string t.front.t
  | _ -> "[NoTile]"

let frame_to_string = function
  | NoFrame -> "NoFrame"
  | Frame (x,y) -> Printf.sprintf "(%d,%d)" x y ;;

let slope_to_string = function
  | Slope sl -> Printf.sprintf "%d" sl
  | HalfBrick -> "HalfBrick"
  | NoSlope -> "NoSlope" ;;

let tile_to_string = function
  | EmptyTile -> "EmptyTile"
  | Tile data -> begin
      let back = data.back in
      let front = data.front in
      (
        Printf.sprintf "\n   Front: type = %s, color = %s, frame = %s, slope = %s"
        (tile_t_to_string front.t) (color_to_string front.color) (frame_to_string front.frame)
        (slope_to_string front.slope)
      ) ^ (
        Printf.sprintf "\n   Back: wire = %s, wall = %s, color = %s, actuator = %b, inactive = %b\n"
        (wire_to_string back.wire) (wall_to_string back.wall) (color_to_string back.color)
        back.actuator back.inactive
      )
  end ;;

let read_tiles in_ch header = 

  Log.infoln "loading tiles...";
  let max_x = int_of_header_data @@ List.assoc "max_tiles_x" header in
  let max_y = int_of_header_data @@ List.assoc "max_tiles_y" header in
  let importance = 
    let unwrap_bools = function 
      | Array a -> Array.map (function Bool b -> b | _ -> false) a
      | _ -> raise @@ Invalid_argument "Cannot unwrap bools of importance!"
    in
    unwrap_bools @@ List.assoc "importance" header
  in 

  let tiles = Array.make_matrix max_y max_x EmptyTile in

  (* A beautiful abstraction over this cluster f*** of a format *)
  let read_tile () : tile_read =

    Log.debugf "begin read tile: pos_in = %d\n" (pos_in in_ch);

    (*let flags_to_string bools = 
      String.concat "," @@ Array.to_list @@ Array.map string_of_bool bools
    in*)
    (* These flags indicate how much we need to read to extract all the
     * information from the series of bytes representing this tile *)
    let flags3,flags2,flags1 = read_tile_flags in_ch in
    (* First byte: b3 (everything past this is optional) *)
    let has_tile_active = flags3.(1) in
    let has_wall = flags3.(2) in
    let has_liquid = flags3.(3) || flags3.(4) in 
    let has_int16_tile_type = flags3.(5) in
    let has_int8_rle = flags3.(6) in
    let has_int16_rle = flags3.(7) in
    (* Second byte: b2 *)
    let has_wire1 = flags2.(1) in
    let has_wire2 = flags2.(2) in
    let has_wire3 = flags2.(3) in
    let slope = int_of_boolbits [|flags2.(4); flags2.(5); flags2.(6); flags2.(7)|] in
    (* Third byte: b1 *)
    let has_actuator = flags1.(1) in
    let has_inactive = flags1.(2) in 
    let has_tile_color = flags1.(3) in
    let has_wall_color = flags1.(4) in
    let has_wire4 = flags1.(5) in

    let tile_type = 
      if has_tile_active && has_int16_tile_type then 
        Solid (read_i16 in_ch)
      else if has_tile_active then 
        Solid (input_byte in_ch)
      else 
        NoType
    in

    let frame = 
      match tile_type with
      | Solid tt when importance.(tt) ->
         (let x = read_i16 in_ch in
          let y = read_i16 in_ch in
          Frame (x,y))
      | _ -> NoFrame
    in

    let tile_color = 
      if has_tile_color
      then Color (input_byte in_ch)
      else NoColor
    in

    let wall =
      if has_wall 
      then Wall (input_byte in_ch)
      else NoWall
    in

    let wall_color = 
      if has_wall && has_wall_color 
      then Color (input_byte in_ch)
      else NoColor
    in

    (* Test for liquid *)
    let tile_type = 
      if has_liquid 
      then Liquid (input_byte in_ch)
      else tile_type
    in

    let wire = 
      if has_wire1 then Wire1
      else if has_wire2 then Wire2
      else if has_wire3 then Wire3
      else if has_wire4 then Wire4
      else NoWire
    in

    let slope = 
      if slope = 1 then HalfBrick
      else if slope = 0 then NoSlope
      else Slope (slope - 1)
    in

    let actuator = has_actuator in
    let inactive = has_inactive in

    let rle_k = 
      if has_int16_rle then read_i16 in_ch
      else if has_int8_rle then input_byte in_ch
      else 0
    in

    let is_empty_tile = 
      tile_type = NoType && 
      frame = NoFrame &&
      wall = NoWall &&
      wire = NoWire && 
      not actuator 
    in

    (* Finally, put the tile together *)
    if is_empty_tile 
    then 
      { t = EmptyTile ;
        k = rle_k }
    else begin
      let foreground = 
        { t = tile_type ;
          color = tile_color ;
          frame = frame ;
          slope = slope } 
      in
      let background =
        { wire = wire ;
          wall = wall ;
          color = wall_color ;
          actuator = actuator ;
          inactive = inactive } 
      in
      { t = Tile { front = foreground ; back = background } ;
        k = rle_k }
    end
  in

  (*Printf.printf "max_x = %d, max_y = %d\n" max_x max_y;*)

  for x=0 to (max_x-1) do
    Log.infof "loading tile row %d/%d\n" (x+1) max_x;
    (*printf.printf "--> new x: %d (max_x = %d, max_y = %d)\n" x max_x max_y;*)
    let rec for_every_y y =
      if y >= max_y then ()
      else begin
        let tile_raw = read_tile () in 
        let rle_count = tile_raw.k in
        let tile = tile_raw.t in

        (* log everything here *)
        Log.debugf "y = %d, x = %d, k = %d, tile = %s\n" y x rle_count (tile_tt_to_string tile);

        (* set tile at y,x *)
        tiles.(y).(x) <- tile;

        (* copy tile data rle times down *)
        for y_with_rle=(y+1) to (y+rle_count) do 
          tiles.(y_with_rle).(x) <- tile
        done;

        (* continue down column *)
        for_every_y (rle_count + y + 1)
      end 
    in
    for_every_y 0;
  done;
  tiles 
;;

let read_chests in_ch = 
  let num_chests = read_i16 in_ch in
  let chest_capacity = read_i16 in_ch in
  let items_to_skip = max (chest_capacity - 40) 0 in (* item overflow *)
  let chest_capacity = min chest_capacity 40 in (* Cap to 40 items *)

  Printf.printf "num_chests = %d, cap=%d (@ %d)\n" num_chests chest_capacity (pos_in in_ch);

  let read_item in_ch = 
    match (read_i16 in_ch) with
    | 0 -> NoItem
    | stack -> 
      begin
        let id = read_i32 in_ch in
        let prefix = input_byte in_ch in
        Printf.printf "Item: stack=%d,id=%ld,prefix=%d\n" stack id prefix;
        Item {id=id;stack=stack;prefix=prefix}
      end
  in

  (*for i=0 to (items_to_skip) do
    if ((read_i16 in_ch) > 0) then
      (ignore @@ read_i32 in_ch;
      ignore @@ input_byte in_ch)
    else ()
  done;*)

  let read_chest i = 
    let x = Int32.to_int @@ read_i32 in_ch in
    let y = Int32.to_int @@ read_i32 in_ch in
    let name = read_pascal_string in_ch in
    Printf.printf "Chest: x=%d,y=%d,name='%s'\n" x y name;
    let item = 
      { pos = { x = x; y = y } ;
        name = name ; 
        items = Array.to_list @@ Array.init chest_capacity (fun i -> read_item in_ch) }
    in
    item
  in

  (* Create array of chests *)
  Array.init num_chests (fun i -> read_chest in_ch)
;;

let read_signs in_ch = () ;;
let read_npcs in_ch = () ;;
let read_entities in_ch = () ;;
let read_pressure_plates in_ch = () ;;

(* let read_world in_ch =
    let header = read_header in_ch in
    let tiles = read_header in_ch in
    let chests = read_header in_ch in
    let signs = read_header in_ch in
    let npcs = read_header in_ch in
    let entities = read_header in_ch in *)

(* Loading functions*)
let world_of_file path = () ;;
let world_of_buffer bytes = () ;;
(* Saving functions *)
let save_world_to_file world = () ;;

Printexc.record_backtrace true
