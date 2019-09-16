(** 
    This module holds all the required printing functions for the game's terminal
    based front end. 
*)

(* Non-printing functions
*******************************************************************************)

(** [cmd_to_string cmd] is the string representation of [cmd] in lowercase. *)
val cmd_to_string : Command.command -> string


(* Main printing functions
*******************************************************************************)

(** [print_red str] prints [str] to stdout in red text. Equivalent to function
    [ANSITerminal.(print_string [red] str)]. *)
val print_red : string -> unit

(** [print_blue str] prints [str] to stdout in blue text. Equivalent to function
    [ANSITerminal.(print_string [blue] str)]. *)
val print_blue : string -> unit

(** [prin_cmd_not_possible cmd] prints a helpful message if a user tries to use
    a command in the wrong area in the game. *)
val print_cmd_not_possible : Command.command -> unit

(* Consider delting, old function  *)
val print_current_team : Game.t -> State.t -> unit 

(** Prints the required message when help is called by the user. The help
    function displays slightly different functions based on what area the user
    is in the game world. (e.g. You can train players in [training_room] so help 
    prints how to train your players, but only does so in [training_room].) *)
val print_help_command : State.location -> unit

(** [print_title_screen ()] Prints the screen that the player sees before they
    load in a game. Prints out the intro graphic defined in
    [graphics/start_screen_logo.txt], then prints out the screen where you type
    in the name of the file you want to load. *)
val print_title_screen : unit -> unit

(** [print_menu adv state] prints the menu containing the player's basic data,
    such as their current position, budget, and team. All information is given
    by [state]. *)
val print_menu : Game.t -> State.t -> unit

(** Prints your updated information after advancing by one day in the game. *)
val print_next_day_info : Game.t -> State.t -> unit

(* TODO Documentation *)
val print_game : Sim.game_result -> unit


(* Table printing functions
*******************************************************************************)

(** [print_single_player player] prints the information of [player] in a table. 
    Clears the terminal of text beofre printing.
    
    Table headers are:
    [Name; Price; Position; Rating; Current Team; Player ID].
    
    After printing the table, stdin will wait for the enter key to be pressed
    before moving on. *)
val print_single_player : Game.single_player -> unit

(** [print_single_team team] Prints the information of [team] in a table.
    Uses two tables: One for displaying the overall information of [team], and
    another for displaying the player composition of [team]. Clears the terminal
    of text before printing.

    Overall team table headers are:
    [Team Name; Description; Team Rating; Team ID].
    Team Composition headers are:
    [Name; Price; Position; Rating; Player ID].

    After printing the table, stdin will wait for the enter key to be pressed
    before moving on. *)
val print_single_team : Game.single_team -> unit

(** [print_shop_table adv state] prints a table of the current players in the
    market defined in [state]. Only prints the players that are not currently in
    a team. 
    
    Table headers:
    [Name; Price; Position; Rating; Player ID]. *)
(* Used in shop_loop function in main.ml *)
val print_shop : Game.t -> State.t -> unit

(** [print_current_team adv state] prints the the information of the player's
    current team composition as defined in [state]. Clears the terminal of text
    before printing.
    
    Table headers are:
    [Name; Price; Position; Rating; Player ID]. *)
(* Used in team_loop function in main.ml *)
val print_team : Game.t -> State.t -> unit

(** [print_arena adv state] prints a table of the current teams in the arena
    defined in [state].
    
    Table Headers:
    [Team Name; Rating] *)
(* Used in arena_loop function in main.ml *)
val print_arena : Game.t -> State.t -> unit

(** [print_train adv state] print (to stdout) a list of the current players
    on the user's team defined in [state]. *)
val print_train : Game.t -> State.t -> unit
