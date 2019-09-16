(** The abstract type representing the simulation state during a game. *)
type t 

(** The type representing outcome of a typical basketball game. 
    For example: {our_score=[14;29;54;78]; opp_score=[21;35;47;67]} denotes a
    game that ended 78-67 in our favor with the scores updated after every 
    quarter.  *)
type game_result = {
  our_score:int list;
  opp_score:int list;
}

(** [init_sim tactic] initializes the sim_t record for playing purposes.  *)
val init_sim : Game.tactic -> t

(** [update_tactic sim_t tactic] updates the tactic that the team will employ in
    upcoming matches and returns the updated sim_t record.  *)
val update_tactic : t -> Game.tactic -> t

(** [sim_match sim_t our_rating opp_rating opp_tactic] sims a game between the two
    teams according to the tactics given and returns the updated sim_t record. *)
val sim_match: t -> Game.team_rating -> Game.team_rating -> Game.tactic -> t

(** [last_result sim_t] returns the scoreline for the last game played by the 
    team. It retuns an empty scoreline if tno game has been played.   *)
val last_result: t -> game_result