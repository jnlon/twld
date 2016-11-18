let inch = open_in Sys.argv.(1) in
let header = World.read_header inch in
List.iter World.print_header_pair header
