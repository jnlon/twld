Printexc.record_backtrace true ;;

let inch = 
  try open_in Sys.argv.(1) 
  with Invalid_argument msg -> (print_endline "Need argv"; exit 1)
in
let header = World.read_header inch in
World.print_header header

(*let tiles = World.read_tiles inch header in
tiles*)
