(** 
   Representation of static game data.

   This module represents the data stored in game files, including opposing 
   teams and their players, players and their stats, special events, and 
   starting budget. It handles loading of that data from JSON as well as 
   querying the data.
*)

(** The abstract type of values representing games. *)

(** The type of team names. *)
type team_name = string

(** The type of player names. *)
type player_name = string

(** The type of tactics a team is employing. It affects the game's result.  *)
type tactic = 
  | UltraDefensive
  | Defensive
  | Balanced
  | Attacking
  | UltraAttacking
  | UnknownTactic

(** Raised when an unknown team is encountered. *)
exception UnknownTeam of team_name

(** Raised when an unknown player is encountered. *)
exception UnknownPlayer of player_name

(** Raised when an unknown tactic is encountered. *)
(* exception UnknownTactic *)

(** type status is the player's status, whether active or rest *)
type status = Active | Rest

(** type player_rating is the player's rating, includes [pace], [shooting], 
    [defending], [passing], [dribbling], [physicality] and [overall] *)
type player_rating = {pace:int; shooting:int; defending:int; 
                      passing:int; dribbling:int;
                      physicality:int; overall:int}

(** type team_rating is the team's rating, includes [attack], [defense], 
    [chemistry] and [overall] *)
type team_rating = {attack:int; defense:int; chemistry:int; overall:int}

(** type [single_player] is the representation of a player. It is made up of
    a [player_name] of type player_name, a [position] of type string, a [rating]
    of type player_rating, a [current_team] of type team_name, a [price] of type int,
    a [player_id] of type int, a [status] of type status, a [level] of type int *)
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
(** type single_team is the representation of a team, it is made up of
    a [team_name] of type team_name, a [description] of type string, a 
    [team_rating] of type team_rating, a [players] of type single_player list, 
    a [team_id] of type int. *)
type single_team = {
  team_name: team_name;
  description: string;
  play_style: string;
  team_rating: team_rating;
  players: single_player list;
  team_id: int;
  tactic:tactic;
}

(** type market is the representation of a market, it is made up of 
    single_player list *)
type market = single_player list

(** type single_event is the representation of a event, it is made up of
    a [event_name] of type string, a [event_reward] of type int, a 
    [event_date] of type int, a [event_id] of type int. *)
type single_event = {
  event_name: string;
  event_reward: int;
  event_date: int;
  event_id: int;
}

(** type market is the representation of a set of events, it is made up of 
    single_events list *)
type special_events = single_event list

(** type t is the representation of a game, it is made up of
    a [team] of type single_team list, a [market] of type market, a 
    [special_events] of type special events, a [budget] of type int, 
    a [date] of type int, a [your_team] of type int *)
type t

(** [get_budget t] is the budget given the data in game [t]. *)
val get_budget : t -> int

(** [get_date t] is the date given the data in game [t]. *)
val get_date: t -> int

(** [get_teams t] is the list of teams given the data in game [t]. *)
val get_teams: t -> single_team list

(** [get_market] is the list of players in the free market given the data in
    game [t]. *)
val get_market: t -> market

(** [special_events t] is the list of events given the data in game [t]. *)
val special_events: t -> special_events

(** [get_your_team t] is the ID of your team given the data in game [t]. *)
val get_your_team : t -> int

(** [get_tactic t] is the tactic of the team given the data in game [t]. *)
val get_tactic : string -> tactic

(** [get_single_player t id] is [x], where [x] is the option of the player
    related to [id], given all players in the data in game [t]. If there is no
    player related to [id], then [x] is [None]. *)
val get_single_player : t -> int -> single_player option

(** [get_single_player' t player_name] is [x], where [x] is the option of the
    player related to [player_name], given all players in the data in game [t].
    If there is no player related to [player_name], then [x] is [None]. *)
val get_single_player' : t -> player_name -> single_player option

(** [from_json j] is the game type having been parsed in from json [j]. *)
val from_json : Yojson.Basic.json -> t

(** [get_single_team t id] is [x], where [x] is the option of the team related
    to [id], given all teams in the data in game [t]. If there is no team
    related to [id], then [x] is [None]. *)
val get_single_team : t -> int -> single_team option

(** [check_player_list' name lst] is [x], where [x] is the option of the
    player related to [name], given the list of players [lst]. If there is no
    player related to [name], then [x] is [None]. *)
val check_player_list' : player_name -> single_player list -> 
  single_player option