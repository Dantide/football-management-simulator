(* Note: You may introduce new code anywhere in this file. *) 

open Yojson.Basic.Util

(* defined in module *)
type team_name = string

(* defined in module *)
type player_name = string

(* defined in module *)
exception UnknownTeam of team_name

(* defined in module *)
exception UnknownPlayer of player_name

(* defined in module *)
type status = Active | Rest

(* defined in module *)
type team_rating = {attack:int; defense:int; chemistry:int; overall:int}

(* defined in module *)
type player_rating = {pace:int; shooting:int; defending:int; 
                      passing:int; dribbling:int;
                      physicality:int; overall:int}

(* defined in module *)
type tactic = 
  | UltraDefensive
  | Defensive
  | Balanced
  | Attacking
  | UltraAttacking
  | UnknownTactic

(* defined in module *)
type single_player = {
  player_name: player_name;
  position: string;
  rating: player_rating;
  current_team: team_name;
  price: int;
  player_id: int;
  status: status;
  level: int;
}

(* defined in module *)
type single_team = {
  team_name: team_name;
  description: string;
  play_style: string;
  team_rating: team_rating;
  players: single_player list;
  team_id: int;
  tactic: tactic;
}

(* defined in module *)
type market = single_player list

(* defined in module *)
type single_event = {
  event_name: string;
  event_reward: int;
  event_date: int;
  event_id: int;
}

(* defined in module *)
type special_events = single_event list

(* defined in module *)
type t = {
  teams: single_team list;
  market: market;
  special_events: special_events;
  budget: int;
  date: int;
  your_team: int;
}

(* defined in module *)
let get_teams t = t.teams

(* defined in module *)
let get_market t = t.market

(* defined in module *)
let special_events t = t.special_events

(* defined in module *)
let get_budget t = t.budget

(* defined in module *)
let get_date t = t.date 

(* defined in module *)
let get_your_team t = t.your_team

(* input the team id and get the team info option *)
(* defined in module *)
let get_single_team t id = 
  let rec helper id lst = 
    match lst with 
    | [] -> None
    | h :: t -> if h.team_id = id then Some h 
      else helper id t
  in helper id (t.teams)

(** [check_player_list id lst] is [x], where [x] is the option of the player
    related to [id], considering only one team's players. If there is no 
    player related to [id] in [lst], then [x] is [None]. *)
let rec check_player_list id = function
  | [] -> None
  | h::t when h.player_id = id -> Some h 
  | _::t -> check_player_list id t

(** [check_all_teams id lst] is [x], where [x] is the option of the player
    related to [id] considering all teams. If there is no player related to 
    [id] in [lst], then [x] is [None]. *)
let rec check_all_teams id = function 
  | [] -> None
  | h::t -> 
    match check_player_list id h.players with 
    | None -> check_all_teams id t
    | Some x -> Some x

(* defined in module *)
let get_single_player t id =
  let all_teams = t.teams in 
  let market_check = check_player_list id t.market in 
  if market_check = None then check_all_teams id all_teams
  else market_check

(** [json_to_single_player_rating json] is the [player_rating] created from a
    valid json object [json].

    Requires: [json] is a valid json representation of a [player_rating]. *)
let json_to_single_player_rating json = 
  {
    pace = json |> member "pace" |> to_int;
    shooting = json |> member "shooting" |> to_int;
    defending = json |> member "defending" |> to_int;
    passing = json |> member "passing" |> to_int;
    dribbling = json |> member "dribbling" |> to_int;
    physicality = json |> member "physicality" |> to_int;
    overall = json |> member "overall" |> to_int;
  } 

(** [json_to_single_team_rating json] is the [team_rating] created from a
    valid json object [json].

    Requires: [json] is a valid json representation of a [team_rating]. *)
let json_to_single_team_rating json = 
  {
    attack = json |> member "attack" |> to_int;
    defense = json |> member "defense" |> to_int;
    chemistry = json |> member "chemistry" |> to_int;
    overall = json |> member "overall" |> to_int;
  }  

(** [json_to_single_player json] is a [single_player] created from a valid json
    object [json].

    Requires: [json] is a valid json representation of a [single_player]. *)
let json_to_single_player json = 
  {
    player_name = json |> member "player name" |> to_string;
    position  = json |> member "position" |> to_string;
    rating = json |> member "rating" |> json_to_single_player_rating;
    current_team = json |> member "current team" |> to_string;
    price = json |> member "price" |> to_int;
    player_id = json |> member "player id" |> to_int;
    status = Rest;
    level = 0;
  }

(** [get_tactic str] is the [tactic] associated with the value of [str]. *)
let get_tactic = function
  | "ultra_defensive" -> UltraDefensive
  | "defensive" -> Defensive
  | "balanced" -> Balanced
  | "attacking" -> Attacking
  | "ultra_attacking" -> UltraAttacking
  | _ -> UnknownTactic

(** [json_to_single_team json] is a [single_team] created from a valid json
    object [json].

    Requires: [json] is a valid json representation of a [single_team]. *)
let json_to_single_team json = 
  {
    team_name = json |> member "team name" |> to_string;
    description = json |> member "description" |> to_string;
    play_style = json |> member "play style" |> to_string;
    team_rating = json |> member "team rating" |> json_to_single_team_rating;
    players = json |> member "players" |> to_list 
              |> List.map json_to_single_player;
    team_id = json |> member "team id" |> to_int;
    tactic = json |> member "play style" |> to_string |> get_tactic;
  }

(** [json_to_single_event json] is a [single_event] created from a valid json
    object [json].

    Requires: [json] is a valid json representation of a [single_event]. *)
let json_to_single_event json = 
  {
    event_name = json |> member "event name" |> to_string;
    event_reward = json |> member "event reward" |> to_int;
    event_date = json |> member "event date" |> to_int;
    event_id = json |> member "event id" |> to_int
  }

(* defined in module *)
let from_json json = 
  {
    teams = json |> member "teams" |> to_list |> List.map json_to_single_team;
    market = json |> member "market" |> to_list 
             |> List.map json_to_single_player; 
    special_events = json |> member "special events" |> to_list 
                     |> List.map json_to_single_event;
    budget = json |> member "budget" |> to_int;
    date = json |> member "date" |> to_int;
    your_team = json |> member "your team" |> to_int;
  }

(** [get_single_team' t team_name] is [x], where [x] is the option of the team
    related to [team_name], considering all teams in game [t]. If there
    is no team related to [team_name], then [x] is [None]. *)
let get_single_team' t name = 
  let rec helper name lst = 
    match lst with 
    | [] -> None
    | h :: t -> if h.team_name = name then Some h 
      else helper name t
  in helper name (t.teams)

(* defined in module *)
let rec check_player_list' name = function
  | [] -> None
  | h::t when h.player_name = name -> Some h 
  | _::t -> check_player_list' name t

(** [check_all_teams' player_name team_lst] is [x], where [x] is the option of
    the player related to [player_name], considering all teams in [team_lst].
    If there is no player related to [player_name], then [x] is [None]. *)
let rec check_all_teams' name = function 
  | [] -> None
  | h::t -> 
    match check_player_list' name h.players with 
    | None -> check_all_teams' name t
    | Some x -> Some x

(* defined in module *)
let get_single_player' t name =
  let all_teams = t.teams in 
  let market_check = check_player_list' name t.market in 
  if market_check = None then check_all_teams' name all_teams
  else market_check
