Printexc.record_backtrace true ;;

let inch = 
  try open_in Sys.argv.(1) 
  with Invalid_argument msg -> (print_endline "Need argv"; exit 1)
in
let header = World.read_header inch in
let tiles = World.read_tiles inch header in
(*World.print_header header;*)
tiles;
World.read_chests inch
