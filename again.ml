(*
TODO :
- deal with double bombing  (DONE)
- well shit ==> dash
- deeper analysis on pathfinfing (n-1/n)
- correct fatal bug on player death (DONE)
- solve when player is on a bomb on an intersection
*)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
Random.self_init () ;;

let debug_all = false ;;
let logg = false ;;

let remaining_dash = ref 0. ;;
let cd = ref 0.0 ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let fodder = ref 0 ;;
let bodder = ref false ;;
type pt = {
  x : int ;
  y : int ;
}

type bomb = {
  xy : pt ;
  size : int ;
  det_time : float ;
}

type player = {
  id : int ;
  xy : pt ;
  nspeed : int ;
  bomb_to_place : int ;
  bomb_radius : int ;
  ndash : int ;
  ntraps : int ;
}

type boost = {
  xy : pt ;
  spec : int ;
}

let default_point = {
  x = 0 ;
  y = 0 ;
} (* works for dead players - inside a wall *)

let default_bomb = {
  xy = default_point ;
  size = 0 ;
  det_time = 0. ;
} 

and default_player = {
  id = -1 ;
  xy = default_point ;
  nspeed = 0 ;
  bomb_to_place = 0 ;
  bomb_radius = 0 ;
  ndash = 0 ;
  ntraps = 0 ;
}

and default_boost = {
  xy = default_point ;
  spec = 0 ;
} 

and useless = ref 0 ;;

let fodder_x = ref 0
and fodder_y = ref 0 ;;

type game_data = {
  mutable dt : float ;
  mutable player_id : int ;
  mutable laby : int array array ;
  mutable nbombs : int ;
  mutable bombs : bomb array ;
  mutable nplayers : int ;
  mutable players : player array ;
  mutable nboosts : int ;
  mutable boosts : boost array ;
}

type danger_map = {
  explosionTimes : (float list) array array ;
  playersTimes : (float list) array array ;
  bonusMap : bool array array ;
  explodedCrates : bool array array ;
}

type moveType = EscapeDeath | BlowUpCrates | KillPlayers | ClaimLand ;;

exception ReturnInt of int ;;
exception ReturnBool of bool ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
let order = [|(-1, 0); (0, 1); (1, 0); (0, -1); (0, 0)|] ;;

let current_status = ref BlowUpCrates ;;
let action = ref 0 ;;

let dash_left = ref 0 ;;

let equal_pt (p1 : pt) (p2 : pt) =
  p1.x = p2.x && p1.y = p2.y ;;

let swap arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j) ;
  arr.(j) <- temp ;;

let is_valid i j len hei =
  i >= 0 && j >= 0 && i < len && j < hei ;;

let print_direction = function
  | 0 -> Printf.fprintf stderr "NORTH "
  | 1 -> Printf.fprintf stderr "EAST  "
  | 2 -> Printf.fprintf stderr "SOUTH "
  | 3 -> Printf.fprintf stderr "WEST  " 
  | 4 -> Printf.fprintf stderr "STILL "
  | _-> failwith "ERROR : invalid direction" ;;

let delta i j =
  if i = j then 1 else 0 ;;

let overwrite_file (filename : string) =
  let ptr = open_out filename in
  close_out ptr ;;

let rec pop_list elt = function
    | [] -> []
    | h::t when h = elt -> t
    | h::t -> h::(pop_list elt t) ;;

let is_empty_lst = function
  | [] -> true
  | _ -> false ;;

let abs = function
  | k when k < 0 -> -k
  | k -> k ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let get_player_id (gd : game_data) (raw_id : int) =
  try
    Array.fold_left (fun i (p : player) -> if p.id = raw_id then raise (ReturnInt i); i+1) 0 gd.players 
  with
    | ReturnInt k -> k ;;

let print_game_data (gd : game_data) =
  Printf.fprintf stderr "--------------------------------| Board data |--------------------------------\n" ;
  Printf.fprintf stderr "Time : %f\n" gd.dt ;
  Printf.fprintf stderr "ID   : %d\n" gd.player_id ;
  Printf.fprintf stderr "Laby [of size %d %d]:\n" (Array.length gd.laby) (Array.length gd.laby.(0));
  for l = 0 to Array.length gd.laby -1 do
    Printf.fprintf stderr "    " ;
    for c = 0 to Array.length gd.laby.(l) -1 do
      Printf.fprintf stderr "%d " gd.laby.(l).(c) ;
    done;
    Printf.fprintf stderr "\n"
  done ;
  Printf.fprintf stderr "Bombs (%d) : \n" gd.nbombs ;
  for b = 0 to gd.nbombs -1 do
    Printf.fprintf stderr "    [Bomb] (at %d %d) (of size %d) (blowing up at %f)\n" gd.bombs.(b).xy.x gd.bombs.(b).xy.y gd.bombs.(b).size gd.bombs.(b).det_time ;
  done;
  Printf.fprintf stderr "Players (%d) : \n" gd.nplayers ;
  for b = 0 to gd.nplayers -1 do
    Printf.fprintf stderr "    [Player %d] (at %d %d) (holding %d %d %d %d %d)\n" gd.players.(b).id gd.players.(b).xy.x gd.players.(b).xy.y gd.players.(b).nspeed gd.players.(b).bomb_to_place  gd.players.(b).bomb_radius gd.players.(b).ndash gd.players.(b).ntraps ;
  done;
  Printf.fprintf stderr "Boosts (%d) : \n" gd.nboosts ;
  for b = 0 to gd.nboosts -1 do
    Printf.fprintf stderr "    [Boost] (at %d %d) (of type %d)\n" gd.boosts.(b).xy.x gd.boosts.(b).xy.y gd.boosts.(b).spec ;
  done;;

let print_dangers (dgs : danger_map) =
  for w = 0 to Array.length dgs.explosionTimes -1 do
    for h = 0 to Array.length dgs.explosionTimes.(0) -1 do
      Printf.fprintf stderr "%d " ((List.length dgs.explosionTimes.(w).(h)) + (List.length dgs.playersTimes.(w).(h))) ;
    done ;
    Printf.fprintf stderr "\n" ;
  done ;;

let print_gain_map (map : int array array) =
  Printf.fprintf stderr "--------------------------------| Gain levels |--------------------------------\n" ;
  for l = 0 to (Array.length map -1) do
    for c = 0 to (Array.length map.(l) -1) do
      Printf.fprintf stderr "%d " map.(l).(c) ;
    done;
    Printf.fprintf stderr "\n"
  done ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let rec ln_b b = function
  | k when k < 0 -> failwith "are you sure about that ?"
  | k when k < b -> 0
  | k -> 1 + ln_b b (k/b) ;;

let int_of_bool = function
  | false -> 0
  | true -> 1 ;;

let get_meta_info (pid : int) =
  let ptr = open_in ("main_"^(string_of_int pid)^".sav") in
  let fct0 () = match (int_of_string (input_line ptr)) with
    | 0 -> current_status := EscapeDeath
    | 1 -> current_status := BlowUpCrates
    | 2 -> current_status := ClaimLand
    | 3 -> current_status := KillPlayers
    | _ -> current_status := EscapeDeath
  in
  fct0 () ;
  try
    let resu = int_of_string (input_line ptr) in
    dash_left := resu -1;
    close_in ptr
  with
    | End_of_file -> close_in ptr ;;

let set_meta_info (pid : int) =
  let ptr = open_out ("main_"^(string_of_int pid)^".sav") in
  let fct0 () = match !current_status with
    | EscapeDeath -> Printf.fprintf ptr "0"
    | BlowUpCrates -> Printf.fprintf ptr "1"
    | ClaimLand -> Printf.fprintf ptr "2"
    | KillPlayers -> Printf.fprintf ptr "3"
  in
  fct0 () ;
  if !dash_left > 0 then
    Printf.fprintf ptr "\n%d" !dash_left ;
  close_out ptr ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let print_integer_aligned (n : int) (dmax : int) =
  let size = 1+ln_b 10 n in
  Printf.fprintf stderr "%d" n ;
  Printf.fprintf stderr "%s" (String.make (dmax-size+1) ' ') ;;

let int_of_string (str : string) =
  String.fold_left (fun acc ch -> let cd = Char.code ch in if cd >= 48 || cd <= 57 then 10*acc + cd - 48 else failwith "not an integer\n") 0 str ;;

let string_of_int (k0 : int) =
  String.make (1) (Char.chr (k0 + 48)) ;;

let int_n_of_string (str : string) (n : int) (nlast : int ref) =
  let res = Array.make n 0 in
  let rec aux idres idstr = match idstr with
    | k when k = String.length str || idres >= n -> 
      nlast := k
    | k ->
      if str.[k] = ' ' then
        aux (idres+1) (k+1)
      else begin
        let cd = Char.code str.[k] in
        if cd >= 48 && cd <= 57 then begin
          res.(idres) <- 10 * res.(idres) + cd - 48 ;
          aux (idres) (k+1)
        end
        else
          failwith "not an integer (n/n)\n"
      end
  in
  aux 0 0 ;
  res ;;

let parse_input (str : string) =
  let ptr = open_in str in
  let (res : game_data) = {dt = 0. ; player_id = 0 ; laby = [||] ; nbombs = 0 ; bombs = [||] ; nplayers = 0 ; players = [||] ; nboosts = 0 ; boosts = [||] ;} in
  try
    (* time *)
    if debug_all then Printf.fprintf stderr "Time\n" ;
    res.dt <- Float.of_string (input_line ptr) ;

    (* player_id *)
    if debug_all then Printf.fprintf stderr "PID\n" ;
    res.player_id <- int_of_string (input_line ptr) ;

    (* maze *)
    if debug_all then Printf.fprintf stderr "Maze\n" ;
    let msize = int_n_of_string (input_line ptr) 2 useless in

    res.laby <- Array.make msize.(0) [||] ;
    for lane = 0 to msize.(0) -1 do
      let psd = input_line ptr in
      res.laby.(lane) <- int_n_of_string psd msize.(1) useless ;
    done;

    (* bombs *)
    if debug_all then Printf.fprintf stderr "Boom\n" ;
    res.nbombs <- int_of_string (input_line ptr) ;

    res.bombs <- Array.make res.nbombs default_bomb ;
    for b = 0 to res.nbombs -1 do
      let psd = input_line ptr
      and last = ref 0 in
      let dat = int_n_of_string psd 3 last in
      let dtime = Float.of_string (String.init (String.length psd - !last) (fun i -> psd.[i + !last])) in
      res.bombs.(b) <- {xy = {x = dat.(0) ; y = dat.(1) ;} ; size = dat.(2) ; det_time = dtime ;
      }
    done;

    (* players *)
    if debug_all then Printf.fprintf stderr "Players\n" ;
    res.nplayers <- int_of_string (input_line ptr) ;

    res.players <- Array.make 4 default_player ;
    for p = 0 to res.nplayers -1 do
      let dat = int_n_of_string (input_line ptr) 8 useless in
      res.players.(dat.(2)) <- {id = dat.(2) ; xy = {x = dat.(0) ; y = dat.(1) ;} ; nspeed = dat.(3) ; bomb_to_place = dat.(4) ; bomb_radius = dat.(5) ; ndash = dat.(6) ; ntraps = dat.(7) ;}
    done;

    (* boosts *)
    if debug_all then Printf.fprintf stderr "Boosts\n" ;
    res.nboosts <- int_of_string (input_line ptr) ;

    res.boosts <- Array.make res.nboosts default_boost ;
    for p = 0 to res.nboosts -1 do
      let dat = int_n_of_string (input_line ptr) 3 useless in
      res.boosts.(p) <- {xy = {x = dat.(0) ; y = dat.(1) ;} ; spec = dat.(2)}
    done;
        
    if debug_all then Printf.fprintf stderr "Done!\n" ;
    close_in ptr ;
    res
  with
    | End_of_file ->
      close_in ptr ;
      failwith "cannot happen unless something is wrong" ;;

let get_rem_dash (filename : string) =
  let ptr = open_in filename in
  remaining_dash := float_of_int (int_of_string (input_line ptr)) ;
  close_in ptr ;;

let set_rem_dash (filename : string) =
  let ptr = open_out filename in
  Printf.fprintf ptr "%d\n" (int_of_float (max 0. (!remaining_dash -. 1.))) ;
  close_out ptr ;;

let build_danger_map (gd : game_data) =
  let lines = Array.length gd.laby 
  and cols = Array.length gd.laby.(0) in
  let (res : danger_map) = {
    explosionTimes = Array.make lines [||] ;
    playersTimes = Array.make lines [||] ;
    bonusMap = Array.make lines [||] ;
    explodedCrates = Array.make lines [||] ;
  } in 
  for l = 0 to lines -1 do
    res.explosionTimes.(l) <- Array.make cols [] ;
    res.playersTimes.(l) <- Array.make cols [] ;
    res.explodedCrates.(l) <- Array.make cols false ;
    res.bonusMap.(l) <- Array.make cols false ;
  done;

  Array.sort
    (
      fun b1 b2 -> int_of_float (100. *. (b1.det_time -. b2.det_time))
    )
    gd.bombs ;

  (*if gd.nbombs > 0 then
    Printf.fprintf stderr "%f %f\n" (gd.bombs.(0).det_time) (gd.bombs.(Array.length gd.bombs -1).det_time) ;*)

  (* add bombs *)
  let halt = ref false in
  for b = 0 to gd.nbombs -1 do
    let bx = gd.bombs.(b).xy.x
    and by = gd.bombs.(b).xy.y in
    let bsize = gd.bombs.(b).size
    and dtime = min (gd.bombs.(b).det_time) (List.fold_left min (gd.dt +. 1000.) res.explosionTimes.(bx).(by)) in
    for dir = 0 to 3 do
      for w = 0 to bsize do
        if (not !halt) && (w > 0 || dir = 0) then begin
          let nx = bx + w * (fst order.(dir))
          and ny = by + w * (snd order.(dir)) in
          if is_valid nx ny lines cols then begin
            if (gd.laby.(nx).(ny) = 0 || gd.laby.(nx).(ny) >= 3) || (gd.laby.(nx).(ny) = 2 && res.explodedCrates.(nx).(ny)) then
              res.explosionTimes.(nx).(ny) <- (dtime)::(res.explosionTimes.(nx).(ny))
            else if gd.laby.(nx).(ny) = 1 then
              halt := true
            else if gd.laby.(nx).(ny) = 2 then begin
              halt := true ;
              res.explodedCrates.(nx).(ny) <- true ;
            end
          end
        end
      done;
      halt := false ;
    done
  done;

  (* add players *)
  for p = 0 to gd.nplayers -1 do
    if p <> gd.player_id then begin
      let bx = gd.players.(p).xy.x
      and by = gd.players.(p).xy.y in
      let bsize = gd.players.(p).bomb_radius
      and dtime = min (gd.dt +. 5.5) (min (List.fold_left min (gd.dt +. 1000.) res.explosionTimes.(bx).(by)) (List.fold_left min (gd.dt +. 1000.) res.playersTimes.(bx).(by))) in
      if dtime <> gd.dt +. 5.5 then begin
        for dir = 0 to 3 do
          for w = 0 to bsize do
            if (not !halt) && (w > 0 || dir = 0) then begin
              let nx = bx + w * (fst order.(dir))
              and ny = by + w * (snd order.(dir)) in
              if is_valid nx ny lines cols then begin
                if (gd.laby.(nx).(ny) = 0 || gd.laby.(nx).(ny) >= 3) || (gd.laby.(nx).(ny) = 2 && res.explodedCrates.(nx).(ny)) then
                  res.playersTimes.(nx).(ny) <- (dtime)::(res.playersTimes.(nx).(ny))
                else if gd.laby.(nx).(ny) = 1 then
                  halt := true
                else if gd.laby.(nx).(ny) = 2 then begin
                  halt := true ;
                  res.explodedCrates.(nx).(ny) <- true ;
                end
              end
            end
          done;
          halt := false ;
        done
      end
    end
  done;

  (* add bonuses *)
  for b = 0 to gd.nboosts -1 do
    res.bonusMap.(gd.boosts.(b).xy.x).(gd.boosts.(b).xy.y) <- true ;
  done;
  res ;;

let is_worth (gd : game_data) (bsize : int) =
  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in
  let cxi = gd.players.(gd.player_id).xy.x
  and cyi = gd.players.(gd.player_id).xy.y in
  let halt = ref false in
  let count = ref 0 in
  try
    if gd.laby.(cxi).(cyi) = 1 || gd.laby.(cxi).(cyi) = 2 then
      raise (ReturnBool false);
    for dir = 0 to 3 do
      for w = 0 to bsize do
        if not !halt && dir = 0 || w > 0 then begin
          let nx = cxi + w * (fst order.(dir))
          and ny = cyi + w * (snd order.(dir)) in
          if is_valid nx ny lines cols then begin
            if gd.laby.(nx).(ny) = 0 || gd.laby.(nx).(ny) >= 3 && gd.laby.(nx).(ny) <> 3+gd.player_id then begin
              incr count ;
              if !count >= 2 then
                raise (ReturnBool true)
            end
            else if gd.laby.(nx).(ny) = 1 || gd.laby.(nx).(ny) = 2 then
              halt := true
          end
        end
      done ;
      halt := false
    done ;
    false
  with 
    | ReturnBool b -> b ;;

let is_worth_pos (gd : game_data) (cxi : int) (cyi : int) (bsize : int) =
  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in
  let halt = ref false in
  try
    if gd.laby.(cxi).(cyi) = 1 || gd.laby.(cxi).(cyi) = 2 then
      raise (ReturnBool false);
    for dir = 0 to 3 do
      for w = 0 to bsize do
        if not !halt && dir = 0 || w > 0 then begin
          let nx = cxi + w * (fst order.(dir))
          and ny = cyi + w * (snd order.(dir)) in
          if is_valid nx ny lines cols then begin
            if gd.laby.(nx).(ny) = 0 || gd.laby.(nx).(ny) >= 3 && gd.laby.(nx).(ny) <> 3+gd.player_id then
              raise (ReturnBool true)
            else if gd.laby.(nx).(ny) = 1 || gd.laby.(nx).(ny) = 2 then
              halt := true
          end
        end
      done ;
      halt := false
    done ;
    false
  with 
    | ReturnBool b -> b ;;

let generate_gain_map (gd : game_data) (dgs : danger_map) =
  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in
  let bsize = gd.players.(gd.player_id).bomb_radius in
  let res = Array.make_matrix lines cols 0 in

  (* aim towards center by adding a bonus (no) *)
  for i = 0 to lines -1 do
    for j = 0 to cols -1 do
      if false && is_worth_pos gd i j gd.players.(gd.player_id).bomb_radius then
      res.(i).(j) <- res.(i).(j) + (min (min (i) (lines -1-i)) (min (j) (cols -1-j))) ;
    done
  done ;

  (* add potential score *)
  for l = 0 to lines -1 do
    for c = 0 to cols -1 do
      if (gd.laby.(l).(c) >= 3) || gd.laby.(l).(c) = 0 then begin
        let halt = ref false in
        for dir = 0 to 3 do
          for w = 0 to bsize do
            if dir = 0 || w > 0 then begin
              let nx = l + w * (fst order.(dir))
              and ny = c + w * (snd order.(dir)) in
              if not !halt && is_valid nx ny lines cols then begin
                if gd.laby.(nx).(ny) = 1 || gd.laby.(nx).(ny) = 2 || Array.exists (fun (b : bomb) -> b.xy.x = nx && b.xy.y = ny) gd.bombs then
                  halt := true
                else if gd.laby.(nx).(ny) <> 3+gd.player_id then begin
                  res.(l).(c) <- res.(l).(c) +1 ;
                  if gd.laby.(nx).(ny) <> 0 then
                    res.(l).(c) <- res.(l).(c) +1 ;
                end
              end
            end
          done ;
          halt := false ;
        done
      end
    done
  done;

  (* AGGRO *)
  for l = 0 to (-1)*gd.nplayers -1 do
    if gd.players.(l).id <> -1 && gd.players.(l).id <> gd.player_id then begin
      let px = gd.players.(l).xy.x
      and py = gd.players.(l).xy.x in
      if gd.laby.(px).(py) <> 1 && gd.laby.(px).(py) <> 2 then begin
        res.(px).(py) <- res.(px).(py) + 3*gd.players.(gd.player_id).ndash * (int_of_bool (gd.players.(gd.player_id).ntraps > 0))
      end
    end
  done;
  res ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let simulate_bomb_deconstruct (gd : game_data) (dgs : danger_map) (bx : int) (by : int) (bsize : int) (dtime0 : float) =
  let saved_data = Hashtbl.create 40 in
  let dtime = min dtime0 (List.fold_left min (32760.) dgs.explosionTimes.(bx).(by)) in
  let lines = Array.length dgs.explosionTimes
  and cols = Array.length dgs.explosionTimes.(0) in
  for dir = 0 to 3 do
    for w = 0 to bsize do
      if (w > 0 || dir = 0) then begin
        let nx = bx + w * (fst order.(dir))
        and ny = by + w * (snd order.(dir)) in
        if is_valid nx ny lines cols then begin
          Hashtbl.add saved_data (nx, ny) dtime ;
          dgs.explosionTimes.(nx).(ny) <- (dtime)::(dgs.explosionTimes.(nx).(ny))
        end
      end
    done;
  done;
  let pid = gd.player_id in
  Array.iter (fun (pl : player) ->
    if pl.id <> pid then begin
      let dtimep = min (gd.dt +. 5.5) (List.fold_left min (32760.) dgs.explosionTimes.(pl.xy.x).(pl.xy.y)) in
      for dir = 0 to 3 do
        for w = 0 to bsize do
          if (w > 0 || dir = 0) then begin
            let nx = pl.xy.x + w * (fst order.(dir))
            and ny = pl.xy.y + w * (snd order.(dir)) in
            if is_valid nx ny lines cols then begin
              Hashtbl.add saved_data (nx, ny) dtimep ;
              dgs.explosionTimes.(nx).(ny) <- (dtimep)::(dgs.explosionTimes.(nx).(ny))
            end
          end
        done;
      done;
    end
  )
  gd.players ;
  saved_data ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let tile_distance (gd : game_data) (x0 : int) (y0 : int) (end_x : int) (end_y : int) =
  (* returns -1 if the 2 tiles cannot connect *)
  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in
  let visited = Hashtbl.create 100 in
  let q = Queue.create () in

  Queue.add (x0, y0, 0) q;

  try
    while not (Queue.is_empty q) do
      let (x, y, depth) = Queue.pop q in
      if is_valid x y lines cols && gd.laby.(x).(y) <> 1 && gd.laby.(x).(y) <> 2 then begin
        if Hashtbl.find_opt visited (x, y) = None then begin                               (* has not been visited yet *)
          Hashtbl.add visited (x, y) 1 ;
          if (x = end_x && y = end_y) then begin
            raise (ReturnInt depth)
          end;
          if true then begin
            for dir = 0 to 3 do
              Queue.add (x + (fst order.(dir)), y + (snd order.(dir)), depth+1) q ;
            done;
          end
        end
      end
    done;
    (-1) ;
  with
    | ReturnInt k -> k ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let d_len = 1 ;;
let min_dist_from_player (gd : game_data) (x : int) (y : int) =
  Array.fold_left min 999999 (Array.init
    (Array.length gd.players)
    (fun i ->
      if gd.player_id = gd.players.(i).id then
        999999
      else begin
        let d = tile_distance gd x y (gd.players.(i).xy.x) (gd.players.(i).xy.y) in
        if d = -1 then 999999 else d
      end
    )
  ) ;;

let min_dist_with_dash (gd : game_data) (x : int) (y : int) =
  Array.fold_left min 999999 (Array.init
    (Array.length gd.players)
    (fun i ->
      if gd.player_id = gd.players.(i).id then
        999999
      else begin
        let d = tile_distance gd x y (gd.players.(i).xy.x) (gd.players.(i).xy.y) in
        if d = 0 then 0 else
        if d = -1 then 999999 else max 1 (d - 3*gd.players.(i).ndash)
      end
    )
  ) ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let amt_free_adj_spaces (gd : game_data) (x : int) (y : int) =
  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in
  Array.fold_left 
    (fun acc (ox, oy) ->
      if not (ox = 0 && oy = 0) && (is_valid (x+ox) (y+oy) lines cols && gd.laby.(x+ox).(y+oy) <> 1 && gd.laby.(x+ox).(y+oy) <> 2 && not (Array.fold_left (fun acc (b : bomb) -> acc || (b.xy.x = x+ox && b.xy.y = y+oy)) false gd.bombs)) then acc+1 else acc
    )
    0
    order ;;

let generate_dead_end_map (gd : game_data) =
  (* links every cell that has at least 3 neighbors *)

  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in
  let res = Array.make_matrix lines cols 72727 in
  (* distances for each tile to exit to a safe place (ie not a dead end) *)

  let pid = gd.player_id in
  let visited = Hashtbl.create (lines * cols) in
  
  (* places 0s to connectable tiles *)
  let rec dfs x y prev last_dir fpass =
    if 
      (is_valid x y lines cols) && 
      (Hashtbl.find_opt visited (x, y) = None) &&
      (gd.laby.(x).(y) <> 1 && gd.laby.(x).(y) <> 2) && 
      ((x = gd.players.(pid).xy.x && y = gd.players.(pid).xy.y) || not (Array.fold_left (fun acc (b : bomb) -> acc || (b.xy.x = x && b.xy.y = y)) false gd.bombs))(*&&
      (not (Array.fold_left (fun acc (p : player) -> acc || (p.xy.x = x && p.xy.y = y && p.id <> pid)) false gd.players))*)
    then begin
      if not (x = gd.players.(pid).xy.x && y = gd.players.(pid).xy.y) then
        Hashtbl.add visited (x, y) 1 ;
      if (amt_free_adj_spaces gd x y) >= 3 || (x = gd.players.(pid).xy.x && y = gd.players.(pid).xy.y) then begin
        if prev <> [] then
          Hashtbl.remove visited (x, y) ;
        List.iter (fun (cx, cy) -> res.(cx).(cy) <- 0) ((x, y)::prev) ;
        for dir = 0 to 3 do
          if fpass <> 0 || (dir + 2) mod 4 <> last_dir then (* dont backtrack *)
            dfs (x + fst (order.(dir))) (y + snd (order.(dir))) [] dir (max 0 (fpass-1))
        done
      end
      else begin
        for dir = 0 to 3 do
          if (dir + 2) mod 4 <> last_dir then
            dfs (x + fst (order.(dir))) (y + snd (order.(dir))) ((x, y)::prev) dir fpass
        done
      end
    end
  in
  dfs gd.players.(pid).xy.x gd.players.(pid).xy.y [] 4 1;

  (* fills remaining spaces with BFS *)
  let bfs (x0 : int) (y0 : int) =
    let q = Queue.create () in
    let visit_mem = Hashtbl.create 100 in
    Queue.add (x0, y0, 0) q ;

    try
      while not (Queue.is_empty q) do
        let (x, y, d) = Queue.pop q in
        if 
          (is_valid x y lines cols) && 
          (Hashtbl.find_opt visit_mem (x, y) = None) &&
          (gd.laby.(x).(y) <> 1 && gd.laby.(x).(y) <> 2) && 
          ((x = gd.players.(pid).xy.x && y = gd.players.(pid).xy.y) || not (Array.fold_left (fun acc (b : bomb) -> acc || (b.xy.x = x && b.xy.y = y)) false gd.bombs))(* &&
          (not (Array.fold_left (fun acc (p : player) -> acc || (p.xy.x = x && p.xy.y = y && p.id <> pid)) false gd.players))*)
        then begin
          Hashtbl.add visit_mem (x, y) 1;
          if res.(x).(y) = 0(* && not (x = gd.players.(pid).xy.x && y = gd.players.(pid).xy.y) *)then
            raise (ReturnInt d) ;
          for dir = 0 to 3 do
            Queue.add (x + fst (order.(dir)), y + snd (order.(dir)), d+1) q
          done
        end
      done;
      ()
    with
      | ReturnInt k -> res.(x0).(y0) <- k
  in
  for l = 0 to lines -1 do
    for c = 0 to cols -1 do
      if res.(l).(c) <> 0 then
        bfs l c
    done
  done;
  res ;;

let is_player_nearby (gd : game_data) (x0 : int) (y0 : int) (detect_dist : int) =
  let mind = min_dist_from_player gd x0 y0 in
  if x0 = gd.players.(gd.player_id).xy.x && y0 = gd.players.(gd.player_id).xy.y then Printf.fprintf stderr "[player %d] %d\n" gd.player_id mind ;
  (mind <= detect_dist) ;;

let reverse_simulate_bomb (dgs : danger_map) (save : (int * int, float) Hashtbl.t) =
  Hashtbl.iter
    (fun (x, y) dt -> 
      dgs.explosionTimes.(x).(y) <- pop_list dt (dgs.explosionTimes.(x).(y))
    )
    save ;;

let is_dead (dgs : danger_map) (x : int) (y : int) (t : float) (dt : float) =
  (List.fold_left (* bombs *)
    (fun acc expl_time ->
      acc || (t >= expl_time && t <= expl_time +. dt)(**|| (t >= expl_time -. dt && t <= expl_time)*)
    )
    false
    dgs.explosionTimes.(x).(y)
  ) || (List.fold_left (* player-related bombs (only if in range of another bomb) *)
    (fun acc expl_time ->
      acc || (t >= expl_time && t <= expl_time +. dt)
    )
    false
    dgs.playersTimes.(x).(y)
  ) ;;

let is_dead_2 (dgs : danger_map) (x : int) (y : int) (t : float) (dt : float) =
  (List.fold_left 
    (fun acc expl_time ->
      acc || (t >= expl_time && t <= expl_time +. dt)(* || (t >= expl_time -. dt && t <= expl_time)*)
    )
    false
    dgs.explosionTimes.(x).(y)
  ) ;;

let is_dead_all (dgs : danger_map) (x : int) (y : int) (t : float) (dt : float) = function
  | true -> is_dead_2 dgs x y t dt 
  | false -> is_dead dgs x y t dt ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let contains_crate (gd : game_data) =
  Array.fold_left
    (fun b1 lst -> b1 || (Array.fold_left (fun b2 tile -> b2 || (tile = 2)) false lst)) false gd.laby ;;

let is_a_crate_nearby (gd : game_data) (dgs : danger_map) =
  let pid = gd.player_id 
  and lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in
  try
    let halt = ref false in
    let res = ref false in
    for dir = 0 to 3 do
      for o = 1 to gd.players.(pid).bomb_radius do
        if not !halt then begin
          let nx = gd.players.(pid).xy.x + o * (fst order.(dir))
          and ny = gd.players.(pid).xy.y + o * (snd order.(dir)) in
          if is_valid nx ny lines cols then begin
            if gd.laby.(nx).(ny) = 2 then begin
              if not dgs.explodedCrates.(nx).(ny) then
                res := true;
              halt := true
            end
            else if gd.laby.(nx).(ny) = 1 then
              halt := true
            else if dgs.bonusMap.(nx).(ny) then
              raise (ReturnBool false)
          end
        end
      done;
      halt := false ;
    done;
    !res
  with
    | ReturnBool b -> b ;;
    
let sees_a_crate (gd : game_data) (dgs : danger_map) (x : int) (y : int) =
  let pid = gd.player_id 
  and lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in
  try
    let halt = ref false in
    let res = ref false in
    for dir = 0 to 3 do
      for o = 1 to gd.players.(pid).bomb_radius do
        if not !halt then begin
          let nx = x + o * (fst order.(dir))
          and ny = y + o * (snd order.(dir)) in
          if is_valid nx ny lines cols then begin
            if gd.laby.(nx).(ny) = 2 then begin
              if not dgs.explodedCrates.(nx).(ny) then
                res := true;
              halt := true
            end
            else if gd.laby.(nx).(ny) = 1 then
              halt := true
            else if dgs.bonusMap.(nx).(ny) then
              raise (ReturnBool false)
          end
        end
      done;
      halt := false ;
    done;
    !res
  with
    | ReturnBool b -> b ;;

let bfs_for_crate ?return_x:(retx=fodder) ?return_y:(rety=fodder) ?return_ok:(retfl=bodder) (gd : game_data) (dgs : danger_map) (x0 : int) (y0 : int) (stime : float) (searchCrate : bool) (searchBonus : bool) (placedBomb : bool) (minDist : int) (ignorePlayers : bool) (maxDist : int) =
  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in

  let visited = Hashtbl.create 100 in
  let q = Queue.create () in
  let interval = Float.pow (0.9) (float_of_int gd.players.(gd.player_id).nspeed) in
  let pid = gd.player_id in

  let needs_gtfo = not (is_empty_lst dgs.explosionTimes.(x0).(y0)) in
  (*let nearest_player = min_dist_with_dash gd gd.players.(pid).xy.x gd.players.(pid).xy.y in*)
  let nearest_player = min_dist_from_player gd gd.players.(pid).xy.x gd.players.(pid).xy.y in

  let undead_end_tiles = generate_dead_end_map gd in

  if gd.player_id = 4 then begin
    for l = 0 to Array.length undead_end_tiles -1 do
      for c = 0 to Array.length undead_end_tiles.(l) -1 do
        if undead_end_tiles.(l).(c) >= 727 then
          Printf.fprintf stderr "- "
        else
          Printf.fprintf stderr "%d " undead_end_tiles.(l).(c);
      done;
      Printf.fprintf stderr "\n";
    done
  end ;

  Queue.add (x0, y0, stime +. interval, 4, 1) q ;

  Queue.add (x0+1, y0, stime +. interval, 2, 1) q ; 
  Queue.add (x0-1, y0, stime +. interval, 0, 1) q ; 
  Queue.add (x0, y0+1, stime +. interval, 1, 1) q ; 
  Queue.add (x0, y0-1, stime +. interval, 3, 1) q ;

  try
    while not (Queue.is_empty q) do
      let (x, y, ct, direction, polar) = Queue.pop q in
      (*Printf.fprintf stderr "at (%d %d)\n" x y;*)
      if is_valid x y lines cols && gd.laby.(x).(y) <> 1 && gd.laby.(x).(y) <> 2 then begin       (* within the map *)
        if Hashtbl.find_opt visited (x, y, polar) = None then begin                               (* has not been visited yet *)
          Hashtbl.add visited (x, y, polar) 1 ;
          if 
            not (is_dead_all dgs x y ct interval ignorePlayers) && 
            ct < stime +. (float_of_int maxDist) *. interval &&
            not (Array.fold_left (fun acc (b : bomb) -> acc || (b.xy.x = x && b.xy.y = y)) false gd.bombs) &&
            polar <= 4
          then begin         (* is not lethal *)
            if false && ct <= stime +. 3. *. interval then begin
              Printf.fprintf stderr "(at %d %d) %b %b %b %b %b\n" x y
              (ct >= stime +. (float_of_int minDist) *. interval)                                 (* not too deep *)
              (is_empty_lst dgs.explosionTimes.(x).(y))                                           (* safe *)
              (needs_gtfo || undead_end_tiles.(x).(y) * 2 <= nearest_player)                      (* is not going to be an ez kill *)
              (not searchCrate || (sees_a_crate gd dgs x y && not dgs.explodedCrates.(x).(y)))    (* sees a crate *)
              (not searchBonus || dgs.bonusMap.(x).(y))                                           (* is a bonus *)
            end;
            if
              (ct >= stime +. (float_of_int minDist) *. interval) &&                              (* not too deep *)
              (is_empty_lst dgs.explosionTimes.(x).(y)) &&                                        (* safe *)
              (needs_gtfo || undead_end_tiles.(x).(y) * 2 <= nearest_player) &&                   (* is not going to be an ez kill *)
              (not searchCrate || (sees_a_crate gd dgs x y && not dgs.explodedCrates.(x).(y))) && (* sees a crate *)
              (not searchBonus || dgs.bonusMap.(x).(y))                                           (* is a bonus *)
            then begin
              retx := x ;
              rety := y ;
              raise (ReturnInt direction)
            end;
            if not (x0 = x && y0 = y) && (needs_gtfo || undead_end_tiles.(x).(y) * 2 <= nearest_player) then begin
              for dir = 0 to 3 do
                Queue.add (x + (fst order.(dir)), y + (snd order.(dir)), ct +. interval, direction, polar) q ;
              done;
              Queue.add (x, y, ct +. interval, direction, polar+1) q
            end
          end
        end
      end
    done;
    retfl := false ;
    4 ;
  with
    | ReturnInt k ->
      retfl := true ;
      k ;;

let rec move_crate (gd : game_data) (dgs : danger_map) =
  let pid = gd.player_id in
  let cxi = gd.players.(pid).xy.x
  and cyi = gd.players.(pid).xy.y in
  let interval = Float.pow 0.9 (float_of_int gd.players.(pid).nspeed) in
  try
    (* send away a player standing right on top *)
    if Array.exists (fun (p : player) -> p.id <> pid && p.xy.x = cxi && p.xy.y = cyi) gd.players && (is_empty_lst dgs.explosionTimes.(cxi).(cyi)) then begin
      if gd.players.(pid).bomb_to_place > 0 then begin
        if logg then Printf.fprintf stderr "oh no you dont\n" ;
        let saved_p = simulate_bomb_deconstruct gd dgs cxi cyi gd.players.(pid).bomb_radius (gd.dt +. 5.5) in

        let bonusres_2p = bfs_for_crate gd dgs cxi cyi (gd.dt -. !remaining_dash *. interval) false true false 0 false 5 in
        if bonusres_2p <> 4 then begin
          if logg then Printf.fprintf stderr "mine (%d) \n" bonusres_2p ;
          raise (ReturnInt bonusres_2p) ;
        end;

        let resultp = bfs_for_crate gd dgs cxi cyi (gd.dt -. !remaining_dash *. interval) false false true 1 false 80 in
        if resultp <> 4 && !action <> 2 then begin
          if logg then Printf.fprintf stderr "go away (%d) \n" resultp ;
          action := 1 ;
          raise (ReturnInt resultp) ;
        end;
        reverse_simulate_bomb dgs saved_p ;
      end
    end;
    if (is_a_crate_nearby gd dgs)(* && (is_empty_lst dgs.explosionTimes.(cxi).(cyi)) *)then begin
      if gd.players.(pid).bomb_to_place > 0 then begin
        if logg then Printf.fprintf stderr "trying...\n" ;
        let saved = simulate_bomb_deconstruct gd dgs cxi cyi gd.players.(pid).bomb_radius (gd.dt +. 5.5) in
        let bonus2_x = ref 0
        and bonus2_y = ref 0 in
        let bonusres_2 = bfs_for_crate ~return_x:bonus2_x ~return_y:bonus2_y gd dgs cxi cyi (gd.dt -. !remaining_dash *. interval) false true false 0 false 8 in
        if bonusres_2 <> 4 && (tile_distance gd cxi cyi !bonus2_x !bonus2_y <= min_dist_from_player gd cxi cyi) && !action <> 2 then begin
          if logg then Printf.fprintf stderr "Bonus Spotted\n" ;
          action := 1 ;
          raise (ReturnInt bonusres_2) ;
        end;

        let result = bfs_for_crate gd dgs cxi cyi (gd.dt -. !remaining_dash *. interval) false false true 1 false 80 in
        if result <> 4 && !action <> 2 then begin
          if logg then Printf.fprintf stderr "found (%d) \n" result ;
          action := 1 ;
          raise (ReturnInt result) ;
        end;
        reverse_simulate_bomb dgs saved ;
      end
    end;
    if logg then Printf.fprintf stderr "bonusing...\n" ;
    let bonus_x = ref 0
    and bonus_y = ref 0 in
    let bonusres = bfs_for_crate ~return_x:bonus_x ~return_y:bonus_y gd dgs cxi cyi (gd.dt -. !remaining_dash *. interval) false true false 0 false 8 in
    if bonusres <> 4 && (tile_distance gd cxi cyi !bonus_x !bonus_y <= min_dist_from_player gd cxi cyi) then begin
      if logg then Printf.fprintf stderr "bonus spotted (%d) \n" bonusres ;
      raise (ReturnInt bonusres) ;
    end;
    if logg then Printf.fprintf stderr "searching...\n" ;
    let rescr = bfs_for_crate gd dgs cxi cyi (gd.dt -. !remaining_dash *. interval) true false false 0 false 80 in
    if logg then Printf.fprintf stderr "searching Done (%d) ...\n" rescr ;
    if rescr <> 4 then
      rescr
    else begin
      if logg then Printf.fprintf stderr "searching 2...\n" ;
      let success = ref false in
      let rescr2 = bfs_for_crate ~return_ok:success gd dgs cxi cyi (gd.dt -. !remaining_dash *. interval) false false false 0 false 80 in
      if logg then Printf.fprintf stderr "searching 2 Done (%d) ...\n" rescr2 ;
      if !success then
        rescr2
      else begin
        if logg then Printf.fprintf stderr "ignoring players...\n" ;
        let rescrip2 = bfs_for_crate ~return_ok:success gd dgs cxi cyi (gd.dt -. !remaining_dash *. interval) false false false 0 true 80 in
        if logg then Printf.fprintf stderr "ignoring players Done (%d)...\n" rescrip2 ;
        if !success then
          rescrip2
        else begin
          if logg then Printf.fprintf stderr "Needs dash lmao\n";
          if !remaining_dash <> 0. && gd.players.(pid).ndash > 0 then begin 
            if logg then Printf.fprintf stderr "---------------- Lets rewind time for a bit ----------------\n";
            remaining_dash := 3. ;
            action := 2 ;
            move_crate gd dgs 
          end
          else begin
            if logg then Printf.fprintf stderr "Now you're screwed\n" ;
            4
          end
        end
      end
    end
  with
    | ReturnInt k -> k ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let path_seek (gd : game_data) (dgs : danger_map) (stime : float) (x0 : int) (y0 : int) (end_x : int) (end_y : int) (max_dist : int) (retval : bool ref) =
  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in

  let visited = Hashtbl.create 100 in
  let q = Queue.create () in
  let interval = Float.pow (0.9) (float_of_int gd.players.(gd.player_id).nspeed) in

  Queue.add (x0, y0, stime, 4, 0) q ;

  try
    while not (Queue.is_empty q) do
      let (x, y, ct, direction, polar) = Queue.pop q in
      (*Printf.fprintf stderr "at (%d %d)\n" x y;*)
      if is_valid x y lines cols && gd.laby.(x).(y) <> 1 && gd.laby.(x).(y) <> 2 then begin       (* within the map *)
        if Hashtbl.find_opt visited (x, y, polar) = None then begin                               (* has not been visited yet *)
          Hashtbl.add visited (x, y, polar) 1 ;
          if 
            ct <= stime +. interval *. (float_of_int max_dist) &&
            (*not (is_dead_all dgs x y ct interval false) && *) (* peace is not an option *)
            not (Array.fold_left (fun acc (b : bomb) -> acc || (b.xy.x = x && b.xy.y = y)) false gd.bombs) &&
            polar <= 4
          then begin
            if
              (x = end_x && y = end_y)                                                            (* dest tile *)
            then begin
              raise (ReturnInt direction)
            end;
            if polar <> 0 then begin
              for dir = 0 to 3 do
                Queue.add (x + (fst order.(dir)), y + (snd order.(dir)), ct +. interval, direction, polar) q ;
              done;
              Queue.add (x, y, ct +. interval, direction, polar+1) q
            end
            else begin
              Queue.add (x0, y0, stime +. interval, 4, 1) q ;

              Queue.add (x0+1, y0, stime +. interval, 2, 1) q ; 
              Queue.add (x0-1, y0, stime +. interval, 0, 1) q ; 
              Queue.add (x0, y0+1, stime +. interval, 1, 1) q ; 
              Queue.add (x0, y0-1, stime +. interval, 3, 1) q ;
            end
          end
        end
      end
    done;
    retval := false ;
    4 ;
  with
    | ReturnInt k ->
      retval := true ;
      k ;;
let get_connex (gd : game_data) (x0 : int) (y0 : int) =
  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in

  let visited = Hashtbl.create 100 in
  let q = Queue.create () in

  Queue.add (x0, y0) q ;

  while not (Queue.is_empty q) do
    let (x, y) = Queue.pop q in
    (*Printf.fprintf stderr "at (%d %d)\n" x y;*)
    if is_valid x y lines cols && gd.laby.(x).(y) <> 1 && gd.laby.(x).(y) <> 2 then begin       (* within the map *)
      if Hashtbl.find_opt visited (x, y) = None then begin                               (* has not been visited yet *)
        Hashtbl.add visited (x, y) 1 ;
        if 
          true
        then begin
          for dir = 0 to 3 do
            Queue.add (x + (fst order.(dir)), y + (snd order.(dir))) q ;
          done;
        end
      end
    end
  done;
  visited ;;

let open_append (filename : string) =
  let memo = Array.make 45 "e" in
  let id = ref 0 in
  let ptrmem = open_in filename in
  try
    while true do
      memo.(!id) <- input_line ptrmem ;
      incr id;
    done ;
    failwith "oh no...\n"
  with
    | End_of_file ->
      close_in ptrmem ;
      let ptr = open_out filename in
      for k = 0 to !id -1 do
        Printf.fprintf ptr "%s\n" memo.(k)
      done;
      ptr ;;

let print_ret (filename : string) (dir : int) (act : int) (do_esc : bool ref) =
  let ptr = open_append filename in
  if !do_esc then
    Printf.fprintf ptr "%d %d\n" dir act
  else begin
    Printf.fprintf ptr "%d %d\n" dir act ;
    do_esc := true
  end ;
  close_out ptr ;;

let is_bomb (dgs: danger_map) (x0 : int) (y0 : int) (dx : int) (dy : int) (dist : int) =
  let lines = Array.length dgs.explosionTimes
  and cols = Array.length dgs.explosionTimes.(0) in
  try
    for i = 1 to dist do
      let nx = x0 + i*dx
      and ny = y0 + i*dy in
      if is_valid nx ny lines cols && not (is_empty_lst dgs.explosionTimes.(nx).(ny)) then
        raise (ReturnBool true)
    done;
    false
  with
    | ReturnBool b -> b ;;

let direction_after_trap (gd : game_data) (dgs : danger_map) (x0 : int) (y0 : int) =
  let interval = Float.pow (0.9) (float_of_int gd.players.(gd.player_id).nspeed) in
  let ok = ref false in
  let result = bfs_for_crate ~return_ok:ok gd dgs x0 y0 (gd.dt -. interval *. !remaining_dash) false false false 0 true 80 in
  if logg then Printf.fprintf stderr "trapping result : %d\n" (int_of_bool !ok) ;
  if !ok then
    result
  else
    raise (ReturnBool false) ;;

let remove_bombs (dgs : danger_map) (lst : (int * int, float) Hashtbl.t list) =
  let rec aux = function
    | [] -> ()
    | h::t -> 
      reverse_simulate_bomb dgs h ;
      aux t  in
  aux lst ;;

let closest_boom (dgs : danger_map) (x : int) (y : int) = 
  if logg then Printf.fprintf stderr "%d %d\n" x y ;
  List.fold_left min 999999. dgs.explosionTimes.(x).(y) ;;

let add_player_count (gd : game_data) =
  let res = ref 0 in
  if gd.players.(gd.player_id).ndash <> 1 then begin
    let connex = get_connex gd gd.players.(gd.player_id).xy.x gd.players.(gd.player_id).xy.y in
    for pl = 0 to 3 do
      if 
        gd.players.(pl).id <> -1 && 
        gd.players.(pl).id <> gd.player_id &&
        Hashtbl.find_opt connex (gd.players.(gd.player_id).xy.x, gd.players.(gd.player_id).xy.y) <> None
      then
        res := 1
    done ;
  end;
  !res ;;

let seek_player (gd : game_data) (dgs : danger_map) =
  (* returns whether or not it found a target, and prints if found *)
  (* note : if this triggers then someone WILL die *)
  let pid = gd.player_id in
  let cxi = ref gd.players.(pid).xy.x
  and cyi = ref gd.players.(pid).xy.y in
  let interval = Float.pow (0.9) (float_of_int gd.players.(gd.player_id).nspeed) in

  let player_range = 3 * gd.players.(pid).ndash in

  let print_escape = ref false in
  
  let has_trapped = ref false in
  let break = ref false in
  let found = ref false in
  let simulated_bombs = ref [] in
  try
    if gd.players.(pid).ntraps = 0 || gd.players.(pid).bomb_to_place = 0 then begin
      if logg then Printf.fprintf stderr "No trap/available bomb\n" ;
      raise (ReturnBool false) 
    end ;
    if gd.players.(pid).ndash <= add_player_count gd then begin
      if logg then Printf.fprintf stderr "Saving bombs\n" ;
      raise (ReturnBool false) 
    end ;
    if logg then Printf.fprintf stderr "Can trap\n" ;
    while not !has_trapped do
      for pl = 0 to Array.length gd.players -1 do
        if not !break && (gd.players.(pl).id <> -1 && pl <> pid) && closest_boom dgs gd.players.(pl).xy.x gd.players.(pl).xy.y <= 3. +. gd.dt then begin
          let destx = gd.players.(pl).xy.x
          and desty = gd.players.(pl).xy.y in
          let foundpath = ref false in
          let directn = path_seek gd dgs (gd.dt -. interval *. !remaining_dash) !cxi !cyi destx desty (player_range + int_of_float !remaining_dash) foundpath in
          if !foundpath then begin
            found := true ;
            if logg then Printf.fprintf stderr "Found target (%d) [%d]\n" pl (int_of_float !remaining_dash) ;
            if logg then Printf.fprintf stderr "(%d, %d) --[%d]--> (%d, %d)\n" !cxi !cyi directn destx desty ;
            if destx = !cxi && desty = !cyi then begin
              if logg then Printf.fprintf stderr "    Trapping\n" ;
              print_ret "again_dash.sav" (direction_after_trap gd dgs !cxi !cyi) 3 print_escape ;
              cxi := !cxi + fst order.(directn) ;
              cyi := !cyi + snd order.(directn) ;
              has_trapped := true ;
              break := true ;
            end
            else if !remaining_dash = 0. then begin
              if logg then Printf.fprintf stderr "    Dashing\n" ;
              print_ret "again_dash.sav" directn 2 print_escape ;
              remaining_dash := 3. ;
              cxi := !cxi + fst order.(directn) ;
              cyi := !cyi + snd order.(directn) ;
              break := true ;
            end
            else begin
              if logg then Printf.fprintf stderr "    Moving\n" ;
              print_ret "again_dash.sav" directn 0 print_escape ;
              cxi := !cxi + fst order.(directn) ;
              cyi := !cyi + snd order.(directn) ;
              break := true ;
            end
          end
        end
      done ;
      if not !found then begin
        if logg then Printf.fprintf stderr "No nearby target\n";
        raise (ReturnBool false) ;
      end ;
      break := false ;
      found := false ;
      remaining_dash := max 0. (!remaining_dash -. 1.)
    done ;
    if logg then Printf.fprintf stderr "[%d] Success!\n" pid ;
    remaining_dash := !remaining_dash +. 1. ;
    remove_bombs dgs !simulated_bombs ;
    true
  with
    | ReturnBool b -> 
      remove_bombs dgs !simulated_bombs ;
      b ;;

let read_queue (filename : string) =
  let strings = Array.make 100 "e" in
  let id = ref 0 in
  let ptr = open_in filename in
  try
    while true do
      strings.(!id) <- input_line ptr ;
      incr id
    done;
    false
  with
    | End_of_file ->
      close_in ptr ;
      if strings.(0) <> "e" then begin
        if logg then Printf.fprintf stderr "Reading queue\n";
        let ptr2 = open_out filename in
        for k = 1 to !id -1 do
          Printf.fprintf ptr2 "%s\n" strings.(k)
        done;
        close_out ptr2 ;
        let choices = int_n_of_string strings.(0) 2 useless in
        Printf.printf "%d %d" choices.(0) choices.(1) ;
        if logg then Printf.fprintf stderr "%d %d" choices.(0) choices.(1) ;
        true
      end else begin
        if logg then Printf.fprintf stderr "No queue\n";
        false
      end

let bfs_for_land ?return_x:(retx=fodder) ?return_y:(rety=fodder) ?return_ok:(retfl=bodder) (skip_near : bool) (gd : game_data) (dgs : danger_map) (x0 : int) (y0 : int) (target_x : int) (target_y : int) ?leniency:(lenc=1) (stime : float) (minDist : int) (maxDist : int) =
  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in

  let visited = Hashtbl.create 100 in
  let q = Queue.create () in
  let interval = Float.pow (0.9) (float_of_int gd.players.(gd.player_id).nspeed) in
  let pid = gd.player_id in

  let needs_gtfo = not (is_empty_lst dgs.explosionTimes.(x0).(y0)) in
  (*let nearest_player = min_dist_with_dash gd gd.players.(pid).xy.x gd.players.(pid).xy.y in*)
  let nearest_player = min_dist_from_player gd gd.players.(pid).xy.x gd.players.(pid).xy.y in

  let undead_end_tiles = generate_dead_end_map gd in

  if gd.player_id = 4 then begin
    for l = 0 to Array.length undead_end_tiles -1 do
      for c = 0 to Array.length undead_end_tiles.(l) -1 do
        if undead_end_tiles.(l).(c) >= 727 then
          Printf.fprintf stderr "- "
        else
          Printf.fprintf stderr "%d " undead_end_tiles.(l).(c);
      done;
      Printf.fprintf stderr "\n";
    done
  end ;

  Queue.add (x0, y0, stime +. interval, 4, 1) q ;

  Queue.add (x0+1, y0, stime +. interval, 2, 1) q ; 
  Queue.add (x0-1, y0, stime +. interval, 0, 1) q ; 
  Queue.add (x0, y0+1, stime +. interval, 1, 1) q ; 
  Queue.add (x0, y0-1, stime +. interval, 3, 1) q ;

  try
    while not (Queue.is_empty q) do
      let (x, y, ct, direction, polar) = Queue.pop q in
      (*Printf.fprintf stderr "at (%d %d)\n" x y;*)
      if is_valid x y lines cols && gd.laby.(x).(y) <> 1 && gd.laby.(x).(y) <> 2 then begin       (* within the map *)
        if Hashtbl.find_opt visited (x, y, polar) = None then begin                               (* has not been visited yet *)
          Hashtbl.add visited (x, y, polar) 1 ;
          if 
            not (is_dead_all dgs x y ct interval false) && 
            ct < stime +. (float_of_int maxDist) *. interval &&
            not (Array.fold_left (fun acc (b : bomb) -> acc || (b.xy.x = x && b.xy.y = y)) false gd.bombs) &&
            polar <= 4
          then begin         (* is not lethal *)
            if false && ct <= stime +. 3. *. interval then begin
              Printf.fprintf stderr "(at %d %d) %b %b %b\n" x y
              (ct >= stime +. (float_of_int minDist) *. interval)                                 (* not too deep *)
              (is_empty_lst dgs.explosionTimes.(x).(y))                                           (* safe *)
              (needs_gtfo || undead_end_tiles.(x).(y) * 2 <= nearest_player)                      (* is not going to be an ez kill *)
            end;
            if
              (ct >= stime +. (float_of_int minDist) *. interval) &&                              (* not too deep *)
              (is_empty_lst dgs.explosionTimes.(x).(y)) &&                                        (* safe *)
              (needs_gtfo || undead_end_tiles.(x).(y) * 2 <= nearest_player) &&                   (* is not going to be an ez kill *)
              (skip_near || tile_distance gd x y target_x target_y <= lenc)                       (* close enough to target *)
            then begin
              retx := x ;
              rety := y ;
              raise (ReturnInt direction)
            end;
            if not (x0 = x && y0 = y) && (needs_gtfo || undead_end_tiles.(x).(y) * 2 <= nearest_player) then begin
              for dir = 0 to 3 do
                Queue.add (x + (fst order.(dir)), y + (snd order.(dir)), ct +. interval, direction, polar) q ;
              done;
              Queue.add (x, y, ct +. interval, direction, polar+1) q
            end
          end
        end
      end
    done;
    retfl := false ;
    4 ;
  with
    | ReturnInt k ->
      retfl := true ;
      k ;;

let rec move_land (gd : game_data) (dgs : danger_map) (gn : int array array) =
  let max_cols = Array.mapi 
    (fun i lne ->
      Array.fold_left
        (fun acc (idc, sco) ->
          if sco > snd acc then
            (idc, sco)
          else
            acc
        )
        (0, 0)
        (Array.mapi (fun j v -> (j, v)) lne)
    )
    gn 
  in
  let (xmax, ymax, _) = Array.fold_left
    (fun (cxm, cym, cvm) (nx, ny, nv) ->
      if nv > cvm then
        (nx, ny, nv)
      else
        (cxm, cym, cvm)
    )
    (0, 0, 0)
    (Array.mapi (fun i (j, v) -> (i, j, v)) max_cols)
  in 
  let pid = gd.player_id in
  let cxi = gd.players.(pid).xy.x
  and cyi = gd.players.(pid).xy.y in
  let interval = Float.pow (0.9) (float_of_int gd.players.(gd.player_id).nspeed) in

  if logg then Printf.fprintf stderr "going at (%d, %d) [score=%d]\n" xmax ymax gn.(xmax).(ymax);

  try
    if gd.players.(pid).bomb_to_place > 0 && Array.exists (fun (p : player) -> p.id <> pid && p.xy.x = cxi && p.xy.y = cyi) gd.players && (is_empty_lst dgs.explosionTimes.(cxi).(cyi)) then begin
      if logg then Printf.fprintf stderr "go away (_2)\n" ;
      let saved_ppl = simulate_bomb_deconstruct gd dgs cxi cyi gd.players.(pid).bomb_radius (gd.dt +. 5.5) in
      let is_good_p = ref false in
      let directn = bfs_for_land ~return_ok:is_good_p true gd dgs cxi cyi xmax ymax (gd.dt -. !remaining_dash *. interval) 1 80 in
      if !is_good_p then begin
        if logg then Printf.fprintf stderr "fire in the hole (%d)\n" directn ;
        action := 1 ;
        raise (ReturnInt directn)
      end;
      reverse_simulate_bomb dgs saved_ppl ;
    end;

    (* try to place a bomb *)
    (*let is_safe = is_empty_lst dgs.explosionTimes.(cxi).(cyi) in*)
    let saved_p = simulate_bomb_deconstruct gd dgs cxi cyi gd.players.(pid).bomb_radius (gd.dt +. 5.5) in
    let is_good = ref false in

    let result_bomb = bfs_for_land ~return_ok:is_good true gd dgs cxi cyi xmax ymax (gd.dt -. !remaining_dash *. interval) 1 80 in
    if (*is_safe && *)gd.players.(pid).bomb_to_place > 0 && !is_good && !action <> 2 && is_worth gd gd.players.(pid).bomb_radius then begin
      if logg then Printf.fprintf stderr "kaboom\n" ;
      action := 1 ;
      result_bomb
    end
    else begin
      reverse_simulate_bomb dgs saved_p ;

      let res = bfs_for_land ~return_ok:is_good false gd dgs cxi cyi xmax ymax (gd.dt -. !remaining_dash *. interval) 1 80 in
      if !is_good then begin
        if logg then Printf.fprintf stderr "going to kaboom\n" ;
        res
      end
      else begin
        if logg then Printf.fprintf stderr "no explosion ?\n" ;
        let res = bfs_for_land ~leniency:20 ~return_ok:is_good false gd dgs cxi cyi xmax ymax (gd.dt -. !remaining_dash *. interval) 1 80 in
        if !is_good then begin
          if logg then Printf.fprintf stderr "found\n";
          res
        end
        else begin
          if logg then Printf.fprintf stderr "Needs dash lmao (_2)\n";
          if !remaining_dash <> 0. && gd.players.(pid).ndash > 0 then begin 
            if logg then Printf.fprintf stderr "-------------- Lets rewind time for a bit (_2) --------------\n";
            remaining_dash := 3. ;
            action := 2 ;
            move_land gd dgs gn
          end
          else begin
            if logg then Printf.fprintf stderr "Now you're screwed (_2)\n" ;
            4
          end
        end
      end
    end 
  with
    | ReturnInt k -> k ;;


(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let exists_crate (gd : game_data) (dgs : danger_map) =
  let lines = Array.length gd.laby
  and cols = Array.length gd.laby.(0) in
  for i = 0 to lines -1 do
    for j = 0 to cols -1 do
      if dgs.explodedCrates.(i).(j) then 
        gd.laby.(i).(j) <- -2 ;
    done
  done ;
  let res = Array.exists
    (fun line -> Array.exists (fun tile -> tile = 2) line)
    gd.laby in
  for i = 0 to lines -1 do
    for j = 0 to cols -1 do
      if dgs.explodedCrates.(i).(j) then 
        gd.laby.(i).(j) <- 2 ;
    done
  done ;
  res ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let get_cd (filename : string) =
  let ptr = open_in filename in
  cd := Float.of_string (input_line ptr) ;
  close_in ptr ;;

let set_cd (nspeed : int) (filename : string) =
  if !remaining_dash = 0. && !action <> 2 then begin
    let ptr = open_out filename in
    let interval = Float.pow 0.9 (float_of_int nspeed) in
    Printf.fprintf ptr "%f\n" (max 0. (!cd -. interval)) ;
    close_out ptr
  end ;;

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)
let __start = Unix.gettimeofday() ;;
let game_map = parse_input "entrees.txt" ;;
if debug_all then print_game_data game_map ;;
let danger_data = build_danger_map game_map ;;
let gain_map = generate_gain_map game_map danger_data ;;

(* ("again"^(string_of_int game_map.player_id)^".sav") *)
get_rem_dash "again_rem.sav" ;;
get_cd "again_cooldown.sav" ;;

(*Printf.fprintf stderr "\n" ;;*)
if game_map.player_id = 727 then begin
  for l = 0 to Array.length gain_map -1 do
    for c = 0 to Array.length gain_map.(l) -1 do
      print_integer_aligned gain_map.(l).(c) 3
    done;
    Printf.fprintf stderr "\n"
  done
end ;;

(*Printf.fprintf stderr "\n" ;;
print_dangers danger_data ;;*)

(*get_meta_info game_map.player_id ;;*)
(*Printf.fprintf stderr "\n" ;;
print_dangers danger_data ;;*)

if read_queue "again_dash.sav" then begin
  remaining_dash := !remaining_dash +. 1. ;
  if logg then Printf.fprintf stderr "[%d] reading...\n" game_map.player_id ;
end
else begin
  let direction = ref 4 in

  if !cd >= 0.01 || not (seek_player game_map danger_data) then begin
    if exists_crate game_map danger_data then begin
      if logg then Printf.fprintf stderr "Crates\n" ;
      direction := move_crate game_map danger_data
    end
    else begin
      if logg then Printf.fprintf stderr "No crates\n" ;
      direction := move_land game_map danger_data gain_map
    end ;

    Printf.printf "%d %d" !direction !action ;
    if logg then Printf.fprintf stderr "[player %d] %d %d (at time %f - with %d dash potential)\n" game_map.player_id !direction !action game_map.dt (int_of_float !remaining_dash);
  end
  else begin
    cd := 3.0;
    ignore (read_queue "again_dash.sav")
  end
end;;

set_rem_dash "again_rem.sav" ;;
set_cd game_map.players.(game_map.player_id).nspeed "again_cooldown.sav" ;;
(*set_meta_info game_map.player_id ;;*)
let __end = Unix.gettimeofday() ;;
if logg then Printf.fprintf stderr "Time : %f\n\n" (__end -. __start) ;;