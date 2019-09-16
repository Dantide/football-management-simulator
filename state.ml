open Game
open Command

(* defined in module *)
type location = 
  | Main
  | Shop
  | Arena
  | Training_Field


(* defined in module *)
type team_name = string

(* defined in module *)
type player_name = string

(* defined in module *)
type t = {
  name: team_name;
  current_location: location;
  current_market: single_player list;
  current_rating: team_rating;
  current_money: int;
  current_date: int;
  current_players_in_team: single_player list;
  current_opponent_teams: single_team list;
  current_energy: int;
  sim_status: Sim.t;
}

(* defined in module *)
type result = Legal of t | Illegal

(* defined in module *)
let get_current_location t =
  t.current_location

(* defined in module *)
let get_current_money t = 
  t.current_money

(* defined in module *)
let get_current_date t = 
  t.current_date

(* defined in module *)
let get_current_players t = 
  t.current_players_in_team |> List.map (fun elt -> elt.player_name)

(* defined in module *)
let get_current_active_players t = 
  t.current_players_in_team |> List.filter (fun elt -> elt.status = Active)

(* defined in module *)
let get_current_team t =
  t.current_players_in_team

(* defined in module *)
let get_current_opponent_teams t = 
  t.current_opponent_teams 
  |> List.sort (fun t1 t2 -> t2.team_rating.overall - t1.team_rating.overall)

(* defined in module *)
let get_current_energy t =
  t.current_energy

(* defined in module *)
let get_current_market t =
  t.current_market |> List.sort (fun p1 p2 -> p1.price - p2.price)

(* defined in module *)
let get_current_sim t =
  t.sim_status

(** [build_t team location market rating money date players progress] creates
    a new state with the given parameters as the fields. *)
let build_t team_name current_location current_market current_rating 
    current_money current_date current_players_in_team 
    current_opponent_teams current_energy sim_status = 
  {
    name = team_name;
    current_location = current_location;
    current_market = current_market;
    current_rating = current_rating;
    current_money = current_money;
    current_date = current_date; 
    current_players_in_team = current_players_in_team;
    current_opponent_teams = current_opponent_teams;
    current_energy = current_energy;
    sim_status = sim_status;
  }

(* defined in module *)
let init_state adv =
  let my_team_info = 
    match get_single_team adv (get_your_team adv) with 
    | None -> failwith "impossible"
    | Some v -> v in
  let current_opponent_teams = 
    adv |> get_teams |> List.filter 
      (fun single_team -> my_team_info.team_name <> single_team.team_name) in
  {
    name = my_team_info.team_name;
    current_location = Main;
    current_market = get_market adv;
    current_rating = my_team_info.team_rating;
    current_money = get_budget adv;
    current_date = get_date adv;
    current_players_in_team = my_team_info.players;
    current_opponent_teams = current_opponent_teams;
    current_energy = 50;
    sim_status = Sim.init_sim my_team_info.tactic;
  }

(* defined in module *)
let next_day st =  
  Legal (build_t st.name st.current_location st.current_market 
           st.current_rating (st.current_money + 100) (st.current_date + 1) 
           st.current_players_in_team 
           st.current_opponent_teams 50 st.sim_status)

(* defined in module *)
let scout = get_single_player'

(** [analysis_helper team_lst team_name] is [x], where [x] is the team
    related to [team_name] in [lst]. If there is no team related to [team_name]
    in [lst], then [x] is [None]. *)
let rec analysis_helper team_lst team_name = 
  match team_lst with 
  | [] -> None
  | h :: t -> if h.team_name = team_name then Some h
    else analysis_helper t team_name

(* defined in module *)
let analysis adv team_name = 
  let my_team = get_single_team adv (get_your_team adv) in
  match my_team with 
  | None -> failwith "impossible"
  | Some h -> analysis_helper (get_teams adv) team_name 

(** [remove player lst] is the list of players after having removed [player]
    from [lst]. *)
let remove (player_info : single_player) (lst : single_player list) = 
  List.filter (fun elt -> elt.player_id <> player_info.player_id) lst

(* defined in module *)
let sell st player_name = 
  match check_player_list' player_name st.current_players_in_team with
  | None -> Illegal
  | Some player -> 
    (* remove [player] from your team *)
    let current_new_players_in_team = 
      remove player st.current_players_in_team in 
    (* update [player]'s information *)
    let new_player_info = {player with player_name = player_name;
                                       current_team = "";
                                       status = Rest} in
    (* add [player] to the market *)
    let current_market = new_player_info :: st.current_market in 
    (* add [player]'s price to your money balance *)
    let current_money = st.current_money + player.price in 
    (* update the game state *)
    Legal (build_t st.name st.current_location current_market 
             st.current_rating current_money st.current_date 
             current_new_players_in_team
             st.current_opponent_teams st.current_energy st.sim_status)

(* defined in module *)
let buy st player_name = 
  match check_player_list' player_name st.current_market with
  | None -> Illegal
  | Some player -> 
    (* update [player]'s information *)
    let new_player_info = {player with player_name = player_name;
                                       current_team = st.name;
                                       status = Rest} in
    (* add [player] to your team *)
    let current_new_players_in_team = 
      new_player_info :: st.current_players_in_team in 
    (* remove [player] from the market *)
    let current_market = remove new_player_info st.current_market in 
    (* deduct [player]'s cost from your money balance *)
    let current_money = st.current_money - player.price in 
    (* if your money is now negative, then don't allow the purchase *)
    if current_money < player.price then Illegal else 
      Legal (build_t st.name st.current_location current_market 
               st.current_rating current_money st.current_date 
               current_new_players_in_team
               st.current_opponent_teams st.current_energy st.sim_status)

(* defined in module *)
let move t location =
  match String.concat "" location with
  | "main" -> Legal(build_t t.name Main t.current_market t.current_rating 
                      t.current_money t.current_date t.current_players_in_team 
                      t.current_opponent_teams 
                      t.current_energy t.sim_status)
  | "shop" -> Legal(build_t t.name Shop t.current_market t.current_rating 
                      t.current_money t.current_date t.current_players_in_team 
                      t.current_opponent_teams 
                      t.current_energy t.sim_status)
  | "arena" -> Legal(build_t t.name Arena t.current_market t.current_rating 
                       t.current_money t.current_date 
                       t.current_players_in_team 
                       t.current_opponent_teams t.current_energy t.sim_status)
  | "training" -> Legal(build_t t.name Training_Field t.current_market 
                          t.current_rating t.current_money t.current_date 
                          t.current_players_in_team 
                          t.current_opponent_teams t.current_energy 
                          t.sim_status)
  | _ -> Illegal

(** [replace player player_lst] is the list of players which [player] replace 
    the object with the same name in [player_lst] *)
let rec replace 
    (player : Game.single_player) 
    (player_lst : Game.single_player list) = 
  match player_lst with 
  | [] -> player_lst
  | h :: t -> if h.player_name = player.player_name then player :: t
    else h :: (replace player t)

(** [check_if_activate_allowed player_lst] is whether [player_lst] has 5 
    activated players or not.  *)
let check_if_activate_allowed (player_lst : Game.single_player list) = 
  let count = List.fold_left 
      (fun acc elt -> if elt.status = Active then acc + 1 else acc) 
      0 player_lst in 
  count < 5

(* defined in module *)
let active st player_name = 
  match check_player_list' player_name st.current_players_in_team with
  | None -> Illegal
  | Some h when h.status = Active -> Illegal
  | Some player ->
    match check_if_activate_allowed st.current_players_in_team with
    | false -> Illegal
    | true -> 
      (* update [player]'s information *)
      let new_player_info = {player with status = Active} in
      (* exchange this updated information with the old information *)
      let current_new_players_in_team = 
        replace new_player_info st.current_players_in_team in 
      Legal (build_t st.name st.current_location st.current_market 
               st.current_rating st.current_money st.current_date 
               current_new_players_in_team
               st.current_opponent_teams st.current_energy st.sim_status)

(* defined in module *)
let rest st player_name = 
  match check_player_list' player_name st.current_players_in_team with
  | None -> Illegal
  | Some h when h.status = Rest -> Illegal
  | Some player ->
    (* update [player]'s information *)
    let new_player_info = {player with status = Rest} in
    (* exchange this updated information with the old information *)
    let current_new_players_in_team = 
      replace new_player_info st.current_players_in_team in 
    Legal (build_t st.name st.current_location st.current_market 
             st.current_rating st.current_money st.current_date 
             current_new_players_in_team 
             st.current_opponent_teams st.current_energy st.sim_status)


(* defined in module *)
let count_stats_num (player_lst : Game.single_player list) = 
  List.fold_left (fun acc p -> 
      if p.position = "defender"  then 
        let def_num = snd acc + 1 in (fst acc, def_num) 
      else if p.position = "forward" then 
        let att_num = fst acc + 1 in (att_num, snd acc)
      else acc
    ) (0, 0) player_lst

(* defined in module *)
let count_stats_ability (player_lst : Game.single_player list) = 
  List.fold_left (fun acc p -> 
      if p.position = "defender" then 
        let def_idx = snd acc + p.rating.overall in (fst acc, def_idx) 
      else if p.position = "forward" then 
        let att_idx = fst acc + p.rating.overall in (att_idx, snd acc)
      else acc
    ) (0, 0) player_lst

(** [updated_attack_rating acc p_lst] is the updated offense team rating of
    all the players in [p_lst] after having trained a player. Tail-recursive. *)
let rec updated_attack_rating acc = function 
  |[] -> acc/20
  |h::t -> (updated_attack_rating (acc + h.rating.shooting + h.rating.dribbling
                                   + h.rating.passing + h.rating.dribbling) t)

(** [updated_defense_rating acc p_lst] is the updated defense team rating of
    all the players in [p_lst] after having trained a player. Tail-recursive. *)
let rec updated_defense_rating acc = function
  | [] -> acc/10
  | h::t -> 
    (updated_defense_rating (acc + h.rating.defending + h.rating.physicality) t)

(** [updated_overall_rating acc p_lst] is the updated overall team rating of
    all the players in [p_lst] after having trained a player. Tail-recursive. *)
let rec updated_overall_rating acc = function
  | [] -> acc
  | h::t -> (updated_overall_rating (acc+ h.rating.overall) t)

(** [updated_team_rating lst] is [x], where [x] represents the team rating
    based off of the players in [lst]. *)
let updated_team_rating new_team chemistry = 
  {chemistry = chemistry;
   attack = updated_attack_rating 0 new_team;
   defense = updated_defense_rating 0 new_team;
   overall = updated_overall_rating 0 new_team}

(** [inc_rating rating] is the updated rating after having trained; all stats
    increase by 2. *)
let inc_rating = function
  | {pace = a; shooting = b; passing = c; dribbling = d; defending = e;
     physicality = f; overall = g} -> 
    {pace = a+2; shooting = b+2; passing = c+2; 
     dribbling = d+2; defending = e+2; physicality = f+2; overall = g+2}

(** [train_helper t player cost] is [r] when attempting to train [player]. 
    If [cost] does not exceed the user's current energy level given in game 
    state [t], and if the monetary cost of training [player] does not exceed
    the user's current money given in game state [t], then [r] is [Legal t'],
    where in [t'], the user now has a deducted energy level and balance, and
    [player] has its upgrade level and rating increased. Otherwise, [r] is
    [Illegal]. *)
let train_helper (t:t) (player:Game.single_player) (energy_cost:int) =
  (* deduct energy cost *)
  let new_energy = t.current_energy - energy_cost in
  (* deduct money cost *)
  let new_money = t.current_money - (3 * player.rating.overall) in
  (* if energy is negative as a result, stop *)
  if new_energy < 0 then Illegal 
  (* if money is negative as a result, stop *)
  else if new_money < 0 then Illegal 
  else
    (* update [player]'s information *)
    let new_player = {player with level = player.level + 1; 
                                  rating = inc_rating player.rating} in
    (* exchange this updated information with the old information *)
    let new_team = replace new_player t.current_players_in_team in 
    (* update the team's rating *)
    let new_rating = updated_team_rating new_team t.current_rating.chemistry in
    Legal(build_t t.name t.current_location t.current_market new_rating
            new_money t.current_date new_team 
            t.current_opponent_teams new_energy t.sim_status)

(* defined in module *)
let train t player =
  match check_player_list' player t.current_players_in_team with 
  | None -> Illegal (* checks if [player] is on the user's team *)
  | Some h ->
    match h.level with
    | num when num < 0 -> Illegal
    | num when num > 4 -> Illegal 
    | num -> train_helper t h (num+6)

(** [challenge_helper t team] checks if the user is able to challenge [team]
    based on their energy level in game state [t]. *)
let challenge_helper t team = 
  (* deduct energy cost *)
  let new_energy = t.current_energy - 30 in 
  if new_energy < 0 then Illegal (* if energy is negative as a result, stop *)
  else let sim_t = Sim.sim_match t.sim_status t.current_rating
           team.team_rating team.tactic in 
    Legal {t with sim_status = sim_t; current_energy = new_energy}

(* defined in module *)
let challenge t (team: Game.team_name) = 
  match analysis_helper t.current_opponent_teams team with 
  | None -> Illegal
  | Some h -> 
    if check_if_activate_allowed (t.current_players_in_team) then Illegal
    else challenge_helper t h

(* defined in module *)
let update t (tactic_name: string) = 
  let tactic = Game.get_tactic tactic_name in 
  match tactic with 
  | UltraDefensive 
  | Defensive
  | Balanced
  | Attacking
  | UltraAttacking -> 
    Legal {t with sim_status = Sim.update_tactic t.sim_status tactic}
  | UnknownTactic -> Illegal

(* defined in module *)
let get_current_shop t = 
  t.current_market |> List.map (fun elt -> elt.player_name)