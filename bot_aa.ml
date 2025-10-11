type minion = {owner:int ; x:int ; y:int ; mutable load:int ; hp:int ; capacity:int ; atk:int} ;;
exception Found of int * int ;;

let ptr = open_in "mapData.txt" in

let mapSize = input_line ptr |> int_of_string in
let (map : (string * int * int) array array) = Array.init mapSize (fun i ->
  input_line ptr
    |> String.split_on_char ' '
    |> List.map (String.split_on_char ',')
    |> List.map (function | (t::r::m::[]) -> (t,int_of_string r,int_of_string m) | _ -> failwith "invalid 1")
    |> Array.of_list
) in
let nMinions = input_line ptr |> int_of_string in
let minions = Array.init nMinions (fun i ->
  input_line ptr
    |> String.split_on_char ','
    |> List.map int_of_string
    |> (function | (o::x::y::l::h::c::a::[]) -> {owner=o ; x=x ; y=y ; load=l ; hp=h ; capacity=c ; atk=a} | _ -> failwith "invalid 2")
) in
let myID, myResources = input_line ptr |> String.split_on_char ' ' |> List.map int_of_string |> (function | (a::b::[]) -> a,b | _ -> failwith "invalid 3") in
let baseX, baseY = input_line ptr |> String.split_on_char ' ' |> List.map int_of_string |> (function | (a::b::[]) -> a,b | _ -> failwith "invalid 4") in

Array.sort (fun m1 m2 -> abs(m2.x-baseX) + abs(m2.y-baseY) -(abs(m1.x-baseX) + abs(m1.y-baseY))) minions;

let is_valid i j =
  0 <= i && i < mapSize && 0 <= j && j < mapSize
in

let friendlies = Hashtbl.create 100 in
Array.iteri (fun i mn -> if mn.owner = myID then Hashtbl.add friendlies (mn.x,mn.y) i) minions;

let order = [(1,0); (0,1); (-1,0); (0,-1)] in
let bfs_simple (cond : int -> int -> string -> int -> int -> bool) (excl : int -> int -> int -> string -> int -> int -> bool) (x0 : int) (y0 : int) =
  let visited = Hashtbl.create 100 in
  let q = Queue.create () in

  let id = Hashtbl.find friendlies (x0,y0) in

  Queue.add (x0,y0,x0,y0) q;
  List.iter (fun (dx,dy) -> Queue.add (x0+dx,y0+dy,x0+dx,y0+dy) q) order;

  try
    while not (Queue.is_empty q) do
      let (cx,cy,mx,my) = Queue.pop q in
      if is_valid cx cy then begin
        if Hashtbl.find_opt visited (cx,cy) = None then begin
          Hashtbl.add visited (cx,cy) 1;
          let (ttype,tval,tmeta) = map.(cx).(cy) in
          if not (excl cx cy id ttype tval tmeta) && ttype <> "W" then begin
            if cond cx cy ttype tval tmeta then
              raise (Found (mx,my));
            if cx <> x0 || cy <> y0 then 
              List.iter (fun (dx,dy) -> Queue.add (cx+dx,cy+dy,mx,my) q) order
          end
        end
      end
    done;
    (*Printf.fprintf stderr "not found\n";*)
    (x0,y0)
  with
    | Found (x,y) -> (x,y)
in

let collide x y id rt rval rmeta =
  Hashtbl.find_opt friendlies (x,y) <> None && Hashtbl.find_opt friendlies (x,y) <> Some id
in

let trivialTrue x y id rt rval rmeta = true
and trivialFalse x y id rt rval rmeta = false in

let money_finder x y rt rval rmeta =
  rval > 0
in

let return_to_depot x y rt rval rmeta =
  x=baseX && y=baseY
in

let ptrOut = open_out "answer.txt" in

Array.iteri (fun i minion ->
  if minion.owner = myID then begin
    if minion.capacity > 0 then begin
      if minion.capacity = minion.load then begin
        let (tx,ty) = bfs_simple return_to_depot trivialFalse minion.x minion.y in
        begin
          match (Hashtbl.find_opt friendlies (tx,ty)) with
            | _ when tx=baseX && ty=baseY -> ()
            | None -> ()
            | Some k -> begin
              let targ = minions.(k) in
              let tot = min minion.load (targ.capacity - targ.load) in
              targ.load <- targ.load + tot
            end
        end;
        Printf.fprintf ptrOut "%d %d %d %d\n" minion.x minion.y tx ty
      end else begin
        let (tx,ty) = bfs_simple money_finder collide minion.x minion.y in
        Printf.fprintf ptrOut "%d %d %d %d\n" minion.x minion.y tx ty
      end
    end
  end
) minions;

if myResources > 3 then begin
  Printf.fprintf ptrOut "CREATE 1 %d 1\n" (min 8 (myResources-1))
end;

close_out ptrOut;
close_in ptr

(*
TYPE_MUR = 'W'
TYPE_NORMAL = 'R'
TYPE_DASH = 'D'
TYPE_SHIELD = 'S'
TYPE_FORCE = 'F'
TYPE_MIDAS = 'M'
TYPE_VITESSE = 'P'
*)