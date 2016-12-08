module List = struct
  include List
  let make size thing = 
    let rec _make i lst = 
      if i = size then lst
      else _make (succ i) (thing :: lst)
    in
    _make 0 [];;

  let repeat_fn fn times =
    let rec _make i lst = 
      if i = times then lst
      else _make (succ i) (fn () :: lst)
    in
    List.rev @@ _make 0 [];;

  let sum lst = 
    List.fold_left (fun a b -> a + b) 0 lst

end;;

module Array = struct
  include Array

  let sum lst = 
    Array.fold_left (fun a b -> a + b) 0 lst ;;

end;;


module Pervasives = struct
  include Pervasives

  let input_byte_array nbytes in_ch = 
    let arr = Array.make nbytes 0 in
    for i=0 to (nbytes-1) do
      arr.(i) <- (input_byte in_ch)
    done;
    arr ;;

  let read_bool in_ch =
    (input_byte in_ch) > 0 ;;

  let read_pascal_string in_ch = 
    let strlen = input_byte in_ch in
    really_input_string in_ch strlen ;;

  (* TODO: Remove duplicate code in read_int *)

  let read_i16 in_ch = 
    let b1 = input_byte in_ch in
    let b2 = input_byte in_ch in
    b1 lor (b2 lsl 8) ;;

  let read_i32 in_ch = 
    let open Int32 in
    let ( ||| ) = logor in
    let ( <<< ) = shift_left in
    let i16_1 = of_int @@ read_i16 in_ch in
    let i16_2 = of_int @@ read_i16 in_ch in
    i16_1 ||| (i16_2 <<< 16) ;;

  let read_i64 in_ch =
    let open Int64 in
    let ( ||| ) = logor in
    let ( <<< ) = shift_left in
    let i32_1 = of_int32 @@ read_i32 in_ch in
    let i32_2 = of_int32 @@ read_i32 in_ch in
    i32_1 ||| (i32_2 <<< 32) ;;

  let read_single in_ch = 
    Int32.to_float @@ read_int32 in_ch ;;

  let read_double in_ch = 
    Int64.to_float @@ read_int64 in_ch ;;

  let read_int32_array in_ch n = 
    Array.init (fun i -> read_int32 in_ch) n ;;

  let read_bool_array in_ch n = 
    Array.init (fun i -> read_bool in_ch) n ;;

  let read_string_array in_ch n = 
    Array.init (fun i -> read_pascal_string in_ch) n ;;

  let read_byte_array in_ch n = 
    Array.init (fun i -> input_byte in_ch) n ;;

  (* Integer exponent *)
  let rec ( ^^ ) base exp =
    if exp = 0 then 1
    else if exp = 1 then base
    else (base*(base ^^ (exp-1))) ;;

end;;

