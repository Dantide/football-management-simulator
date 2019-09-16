(* 
   This module represents the state of a game as it is being played, including 
   the user's current page and functions that cause the state to change.
 *)

(** The type representing the name of a team. *)
type team_name = string

(** The type representing the name of a player. *)
type player_name = string

(** The type representing which menu the user is currently in. *)
type location = 
  | Main
  | Shop
  | Arena
  | Training_Field

(** The abstract type of values representing the game state. *)
type t

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(** [init_state a] is the initial state of the game when playing game [a]. 
    In that state the user is currently directed to the menu page. *)
val init_state : Game.t -> t 

(** [get_current_location t] is the current menu location of the user at the
    given game state [t]. *)
val get_current_location : t -> location

(** [get_current_money t] is the current amount of money the user has at the
    given game state [t]. *)
val get_current_money : t -> int

(** [get_current_date t] is the current date of the game at the given game
    state [t]. *)
val get_current_date : t -> int

(** [get_current_players t] is the list of players currently on the user's
    team at the given game state [t]. *)
val get_current_players : t -> Game.player_name list

(** [get_current_active_players t] is the list of players currently on the 
    user's team and are activated at the given game state [t]. *)
val get_current_active_players : t -> Game.single_player list

(** [get_current_team t] is the list of players in your team *)
val get_current_team : t -> Game.single_player list

(** [get_current_opponent_teams t] is the current opponent team at the given 
    game state [t]. *)
val get_current_opponent_teams: t -> Game.single_team list

(** [get_current_energy t] is the current energy of the user at game state
    [t]. *)
val get_current_energy : t -> int

(** [get_current_market t] is the current shop and its stock at the given game
    state [t]. *)
val get_current_market : t -> Game.single_player list

(* [get_current_sim t] is the current simulation status at the given game
   state [t]. *)
val get_current_sim : t -> Sim.t

(** [next_day t] is the new game state after having updated the given game
    state [t]. The user's money is incremented by 100, and the date increments
    by 1. *)
val next_day : t -> result

(** [scout t name] is the [single_player] associated with [name] in the given
    game state [t]. *)
val scout : Game.t -> Game.player_name -> Game.single_player option

(** [analysis t name] is the [single_team] associated with [name] in the given
    game state [t]. *)
val analysis : Game.t -> Game.team_name -> Game.single_team option

(** [sell t name] is [r] if attempting to sell [name] to the market. If [name] 
    is a current player on the user's team, then [r] is [Legal t'], where in
    [t'], [name] is no longer on the user's team and is now in the market.
    Otherwise, [r] is [Illegal]. *)
val sell : t -> Game.player_name -> result

(** [buy t name] is [r] if attempting to buy [name] from the market. If [name] 
    is currently in the market, then [r] is [Legal t'], where in [t'], 
    [name] is no longer in the market and is now on the user's team.
    Otherwise, [r] is [Illegal]. *)
val buy : t -> Game.player_name -> result

(** [move t location] is [r] if attempting to move to menu [location]. If
    [location] is a valid menu, then [r] is [Legal t'], where in [t'], the
    user is now in menu [location]. Otherwise, [r] is [Illegal]. *)
val move : t -> Command.object_phrase -> result

(** [active t name] is [r] if attempting to activate [name] on the team. If 
    [name] is currently in the team, then [r] is [Legal t'], where in [t'], 
    [name] is no longer in the team and is now on the user's team.
    Otherwise, [r] is [Illegal]. *)
val active : t -> Game.player_name -> result

(** [rest t name] is [r] if attempting to rest [name] from the team. If [name] 
    is currently in the team, then [r] is [Legal t'], where in [t'], 
    [name] is no longer in the team and is now on the user's team.
    Otherwise, [r] is [Illegal]. *)
val rest : t -> Game.player_name -> result

(** [count_stats_num t] is the calculation of the number of each position [t] *)
val count_stats_num : Game.single_player list -> int * int 

(** [count_stats_ability t] is the calculation of the ability 
    of each position [t] *)
val count_stats_ability : Game.single_player list -> int * int 

(** [train t name] is [r] if attempting to train [name]. If [name] is currently
    on the player's team, and if the user has enough energy and money to train 
    [name], and if [name] has a level less than 5, then [r] is [Legal t'], 
    where in [t'], [name] has had its stats increased. Otherwise, [r] is 
    [Illegal]. *)
val train : t -> Game.player_name -> result

(** [challenge t name] is [r] if attempting to challenge [name]. If [name] is 
    currently on the opponent team list, and if the user has enough energy and 
    money to challenge [name], then [r] is [Legal t'], where in [t'], [name] has 
    had its form updated. Otherwise, [r] is [Illegal]. *)
val challenge : t -> Game.team_name -> result

(** [update t name] is [r] if attempting to update [name]. If [name] is 
    a valid tactic, then [r] is [Legal t'], where in [t'], [name] has 
    had its tactic updated. Otherwise, [r] is [Illegal]. *)
val update : t -> string -> result

(** [get_current_shop t] get all players' name in market. *)
val get_current_shop : t -> Game.player_name list