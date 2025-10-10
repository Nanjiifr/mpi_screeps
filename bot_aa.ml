type minion = {owner:int ; x:int ; y:int ; load:int ; hp:int ; capacity:int ; atk:int} ;;

let ptr = open_in "mapData.txt" in

let mapSize = input_line ptr |> int_of_string in
let (map : (string * int * int) array array) = Array.init mapSize (fun i ->
  input_line ptr
    |> String.split_on_char ' '
    |> List.map (String.split_on_char ',')
    |> List.map (fun (t::r::m::[]) -> (t,int_of_string r,int_of_string m))
    |> Array.of_list
) in
let nMinions = input_line ptr |> int_of_string in
let minions = Array.init nMinions (fun i ->
  input_line ptr
    |> String.split_on_char ','
    |> List.map int_of_string
    |> (fun (o::x::y::l::h::c::a::[]) -> {owner=o ; x=x ; y=y ; load=l ; hp=h ; capacity=c ; atk=a})
) in
let myID, myResources = input_line ptr |> String.split_on_char ' ' |> List.map int_of_string |> (fun (a::b::[]) -> a,b) in
let baseX, baseY = input_line ptr |> String.split_on_char ' ' |> List.map int_of_string |> (fun (a::b::[]) -> a,b) in

close_in ptr;

Array.iter (fun line ->
  Array.iter (fun (t,r,m) -> Printf.printf "[%s %d %d]" t r m) line;
  print_endline ""
) map