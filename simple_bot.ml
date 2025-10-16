type minion = {
  owner : int;
  x : int;
  y : int;
  load : int;
  hp : int;
  capacity : int;
  atk : int;
}

let calcDist x1 y1 x2 y2 = abs (x1 - x2) + abs (y1 - y2)

let getEnemiesDist x y enemyPos =
  let dist =
    Array.make_matrix (Array.length enemyPos) (Array.length enemyPos) max_int
  in
  Array.iteri
    (fun i v ->
      Array.iteri
        (fun j tile ->
          match tile with true -> dist.(i).(j) <- calcDist x y i j | _ -> ())
        v)
    enemyPos;
  dist

let getRessourcesDist x y map =
  let dist = Array.make_matrix (Array.length map) (Array.length map) max_int in
  Array.iteri
    (fun i v ->
      Array.iteri
        (fun j tile ->
          match tile with
          | "R", k, _ when k > 0 -> dist.(i).(j) <- calcDist x y i j
          | _ -> ())
        v)
    map;
  dist

let can_enter m newX newY used_spaces ally_pos ally_has_space =
  (not used_spaces.(newX).(newY))
  && ((not ally_pos.(newX).(newY))
     || (m.load = m.capacity && ally_has_space.(newX).(newY)))

let playBestActions m map used_spaces ally_pos ally_has_space baseX baseY
    enemyPos hasEnemies hasResources =
  let x, y = (m.x, m.y)
  and load = m.load
  and capacity = m.capacity
  and attack = m.atk in
  match attack with
  | n when (hasEnemies && n = 5) || not hasResources ->
      let dist = getEnemiesDist x y enemyPos in
      let mDist =
        Array.init (Array.length map) (fun i ->
            let _, minValue, minIndex =
              Array.fold_left
                (fun (idx, cMin, cMinInd) d ->
                  let cMin, cMinInd =
                    if d < cMin then (d, idx) else (cMin, cMinInd)
                  in
                  (idx + 1, cMin, cMinInd))
                (0, max_int, 0) dist.(i)
            in
            (minValue, minIndex))
      in
      let _, (_, targetY), targetX =
        Array.fold_left
          (fun (idx, (cMin, cMinC), cMinInd) (d, ind) ->
            let cMin, cMinC, cMinInd =
              if d < cMin then (d, ind, idx) else (cMin, cMinC, cMinInd)
            in
            (idx + 1, (cMin, cMinC), cMinInd))
          (0, mDist.(0), 0)
          mDist
      in
      let distToTarget =
        [|
          (calcDist (x + 1) y targetX targetY, (x + 1, y));
          (calcDist (x - 1) y targetX targetY, (x - 1, y));
          (calcDist x (y + 1) targetX targetY, (x, y + 1));
          (calcDist x (y - 1) targetX targetY, (x, y - 1));
        |]
      in
      Array.sort (fun (d1, _) (d2, _) -> d1 - d2) distToTarget;
      let flag = ref true in
      let i = ref 0 in
      let actionPlayed = ref (x, y) in
      let n = Array.length map in
      while !i < 4 && !flag do
        let _, (newX, newY) = distToTarget.(!i) in
        if
          0 <= newX && newX < n && 0 <= newY && newY < n
          && can_enter m newX newY used_spaces ally_pos ally_has_space
        then (
          used_spaces.(newX).(newY) <- true;
          flag := false;
          actionPlayed := (newX, newY))
        else incr i
      done;
      !actionPlayed
  | _ -> (
      match capacity - load with
      | n when n > 0 -> (
          match map.(x).(y) with
          | "R", j, _ when j > 0 -> (x, y)
          | _ ->
              let dist = getRessourcesDist x y map in
              let mDist =
                Array.init (Array.length map) (fun i ->
                    let _, minValue, minIndex =
                      Array.fold_left
                        (fun (idx, cMin, cMinInd) d ->
                          let cMin, cMinInd =
                            if d < cMin then (d, idx) else (cMin, cMinInd)
                          in
                          (idx + 1, cMin, cMinInd))
                        (0, max_int, 0) dist.(i)
                    in
                    (minValue, minIndex))
              in
              let _, (_, targetY), targetX =
                Array.fold_left
                  (fun (idx, (cMin, cMinC), cMinInd) (d, ind) ->
                    let cMin, cMinC, cMinInd =
                      if d < cMin then (d, ind, idx) else (cMin, cMinC, cMinInd)
                    in
                    (idx + 1, (cMin, cMinC), cMinInd))
                  (0, mDist.(0), 0)
                  mDist
              in
              let distToTarget =
                [|
                  (calcDist (x + 1) y targetX targetY, (x + 1, y));
                  (calcDist (x - 1) y targetX targetY, (x - 1, y));
                  (calcDist x (y + 1) targetX targetY, (x, y + 1));
                  (calcDist x (y - 1) targetX targetY, (x, y - 1));
                |]
              in
              Array.sort (fun (d1, _) (d2, _) -> d1 - d2) distToTarget;
              let flag = ref true in
              let i = ref 0 in
              let actionPlayed = ref (x, y) in
              let n = Array.length map in
              while !i < 4 && !flag do
                let _, (newX, newY) = distToTarget.(!i) in
                if
                  0 <= newX && newX < n && 0 <= newY && newY < n
                  && can_enter m newX newY used_spaces ally_pos ally_has_space
                then (
                  used_spaces.(newX).(newY) <- true;
                  flag := false;
                  actionPlayed := (newX, newY))
                else incr i
              done;
              !actionPlayed)
      | _ ->
          let targetX, targetY = (baseX, baseY) in
          let distToTarget =
            [|
              (calcDist (x + 1) y targetX targetY, (x + 1, y));
              (calcDist (x - 1) y targetX targetY, (x - 1, y));
              (calcDist x (y + 1) targetX targetY, (x, y + 1));
              (calcDist x (y - 1) targetX targetY, (x, y - 1));
            |]
          in
          Array.sort (fun (d1, _) (d2, _) -> d1 - d2) distToTarget;
          let flag = ref true in
          let i = ref 0 in
          let actionPlayed = ref (x, y) in
          let n = Array.length map in
          while !i < 4 && !flag do
            let _, (newX, newY) = distToTarget.(!i) in
            if
              0 <= newX && newX < n && 0 <= newY && newY < n
              && can_enter m newX newY used_spaces ally_pos ally_has_space
            then (
              used_spaces.(newX).(newY) <- true;
              (* réserver la destination *)
              flag := false;
              actionPlayed := (newX, newY))
            else incr i
          done;
          !actionPlayed)

let () =
  let ptr = open_in "mapData.txt" in

  let mapSize = input_line ptr |> int_of_string in
  let (map : (string * int * int) array array) =
    Array.init mapSize (fun _ ->
        input_line ptr |> String.split_on_char ' '
        |> List.map (String.split_on_char ',')
        |> List.map (function
             | [ t; r; m ] -> (t, int_of_string r, int_of_string m)
             | _ -> failwith "invalid 1")
        |> Array.of_list)
  in
  let nMinions = input_line ptr |> int_of_string in
  let minions =
    Array.init nMinions (fun _ ->
        input_line ptr |> String.split_on_char ',' |> List.map int_of_string
        |> function
        | [ o; x; y; l; h; c; a ] ->
            { owner = o; x; y; load = l; hp = h; capacity = c; atk = a }
        | _ -> failwith "invalid 2")
  in
  let myID, myResources =
    input_line ptr |> String.split_on_char ' ' |> List.map int_of_string
    |> function
    | [ a; b ] -> (a, b)
    | _ -> failwith "invalid 3"
  in
  let baseX, baseY =
    input_line ptr |> String.split_on_char ' ' |> List.map int_of_string
    |> function
    | [ a; b ] -> (a, b)
    | _ -> failwith "invalid 4"
  in
  let curTurn, maxTurn =
    input_line ptr |> String.split_on_char ' ' |> List.map int_of_string
    |> function
    | [ a; b ] -> (a, b)
    | _ -> failwith "invalid 5"
  in
  let _randomEvents =
    input_line ptr |> String.split_on_char ' '
    |> List.map (String.split_on_char ',')
    |> List.map (function
         | [ a; b ] -> (int_of_string a, int_of_string b)
         | _ -> (-1, -1))
    |> List.filter (fun (a, b) -> a <> -1 || b <> -1)
    |> Array.of_list
  in

  close_in ptr;

  (*
TYPE_MUR = 'W'
TYPE_NORMAL = 'R'
TYPE_DASH = 'D'
TYPE_SHIELD = 'S'
TYPE_FORCE = 'F'
TYPE_MIDAS = 'M'
TYPE_VITESSE = 'P'
*)
  let ptrOut = open_out "answer.txt" in

  (* used_spaces: uniquement pour murs + réservations de destinations *)
  let usedSpaces = Array.make_matrix mapSize mapSize false in
  Array.iteri
    (fun i v ->
      Array.iteri
        (fun j tile ->
          match tile with "W", _, _ -> usedSpaces.(i).(j) <- true | _ -> ())
        v)
    map;

  let enemyPos = Array.make_matrix mapSize mapSize false in
  Array.iter
    (fun m -> if m.owner <> myID then enemyPos.(m.x).(m.y) <- true)
    minions;
  let hasEnemies =
    Array.fold_left ( || ) false
      (Array.map (fun v -> Array.fold_left ( || ) false v) enemyPos)
  in

  let resourcesPos = Array.make_matrix mapSize mapSize false in
  Array.iteri
    (fun i v ->
      Array.iteri
        (fun j v' ->
          match v' with
          | "R", k, _ when k > 0 -> resourcesPos.(i).(j) <- true
          | _ -> ())
        v)
    map;
  let hasResources =
    Array.fold_left ( || ) false
      (Array.map (fun v -> Array.fold_left ( || ) false v) resourcesPos)
  in

  let minionCount =
    Array.fold_left
      (fun acc m -> if m.owner = myID then acc + 1 else acc)
      0 minions
  in

  let forrorCount =
    Array.fold_left
      (fun acc m -> if m.owner = myID && m.capacity = 18 then acc + 1 else acc)
      0 minions
  in
  let attackerCount = minionCount - forrorCount in

  let ally_pos = Array.make_matrix mapSize mapSize false in
  let ally_has_space = Array.make_matrix mapSize mapSize false in
  Array.iter
    (fun m ->
      if m.owner = myID then (
        ally_pos.(m.x).(m.y) <- true;
        if m.load < m.capacity then ally_has_space.(m.x).(m.y) <- true))
    minions;

  Array.iter
    (fun minion ->
      if minion.owner = myID then (
        let temp = usedSpaces.(baseX).(baseY) in
        if minion.load = 0 then usedSpaces.(baseX).(baseY) <- true;
        let newX, newY =
          playBestActions minion map usedSpaces ally_pos ally_has_space baseX
            baseY enemyPos hasEnemies hasResources
        in
        usedSpaces.(baseX).(baseY) <- temp;
        Printf.fprintf ptrOut "%d %d %d %d\n" minion.x minion.y newX newY))
    minions;

  if forrorCount < 15 && (curTurn <= 100 || attackerCount > 15) then (
    if myResources >= 10 then Printf.fprintf ptrOut "CREATE 1 9 0\n")
  else if myResources >= 15 then Printf.fprintf ptrOut "CREATE 5 5 5\n";

  close_out ptrOut
