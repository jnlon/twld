Printexc.record_backtrace true ;;

let inch = open_in Sys.argv.(1) in
let header = World.read_header inch in
(*List.iter World.print_header_pair header*)


let tiles = World.read_tiles inch header in
tiles
