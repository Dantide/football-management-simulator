open Command
open State
open Game
open Printer

exception NotLegal

let checkLegal = function 
  | Illegal -> raise NotLegal
  | Legal h -> h

let quit () = ignore(Sys.command "clear"); print_string "Bye!\n"; exit 0

let empty () = print_red "Please enter a command.\n"

let malformed () = print_red "Not a valid command.\n"


let analysis_helper team adv =
  let team_name = (team |> List.fold_left (fun acc elt -> elt ^ "" ^ acc) "") in
  match analysis adv team_name with 
  | None ->
    print_string ("Sorry, there is no such team!\nIf you want to analyze your \
    own team, type 'team'.\n"); ()
  | Some h -> print_single_team h; ()

let scout_helper player adv =
  let player_name = (List.fold_left (fun acc elt -> elt ^ "" ^ acc) "" player) in
  match scout adv player_name with 
  | None -> print_string ("Sorry, there is no such player!\n"); ()
  | Some p -> print_string ("Here is the information about " ^ player_name ^ ":\n");
    print_single_player p; ()



let buy_helper player state =
  let player_name = (player |> String.concat " ") in 
  match checkLegal (buy state player_name) with 
  | exception NotLegal ->
    print_endline ("You cannot buy " ^ player_name ^ "."); state
  | new_st ->
    print_string ("You buy " ^ (player |> String.concat " ") ^ "\n"); new_st

let sell_helper player state =
  let player_name = (player |> String.concat " ") in 
  match checkLegal (sell state player_name) with 
  | exception NotLegal ->
    print_endline ("You cannot sell " ^ player_name ^ "."); state
  | new_st -> 
    print_string ("You sell " ^ (player |> String.concat " ") ^ "\n"); new_st

let challenge_helper team state =
  let team_name = (team |> List.fold_left (fun acc elt -> elt ^ "" ^ acc) "") in 
  match challenge state team_name with
  | Illegal ->
    print_string ("Sorry, either there is no such team, or you have less than \
    five players activated!\n"); state
  | Legal new_st ->
    new_st |> get_current_sim |> Sim.last_result |> print_game; new_st

let update_helper tactic state =
  let tactic_name = (String.concat " " tactic) in 
  match update state tactic_name with
  | Illegal ->
    print_string "Sorry, there is no such tactic! You can try ultra_defensive, \
    defensive, balanced, attacking, and ultra_attacking.\n"; state
  | Legal new_st -> 
    print_red (tactic_name ^ " is now updated. " ^ tactic_name ^ " is your new \
    tactic"); new_st

let activate_helper player state =
  let player_name = (List.fold_left (fun acc elt -> elt ^ "" ^ acc) "" player) in 
  match active state player_name with
  | Illegal -> (* TODO Fix this error message *)
    print_string "Sorry, there is no such player in your team or \
                  you cannot activate an active player or \
                  you cannot activate more than 5 players!\n"; state
  | Legal new_st -> 
    print_red ("Activation successful. " ^ player_name ^ " is now activated.\n");
    new_st

let rest_helper player state = 
  let player_name = (List.fold_left (fun acc elt -> elt ^ "" ^ acc) "" player) in 
  match rest state player_name with
  | Illegal ->
    print_string "Sorry, there is no such player in your team!\n"; state
  | Legal new_st ->
    print_red ("Deactivation successful. " ^ player_name ^ " is now resting.");
    new_st

let train_helper player state =
  let player_name = (String.concat " " player) in 
  match checkLegal (train state player_name) with
  | exception NotLegal -> print_string "You cannot train this player.\n"; state
  | new_st -> print_string ("You train " ^ player_name ^ "\n"); new_st



(** [shop_loop adv state] is the game loop on the shop screen. Reached by
    using commands [move shop] and [shop] from the main screen. Exited by typing
    "back". *)
let rec shop_loop adv state =
  print_string "\n> ";
  match read_line () with
  | "back" -> print_menu adv state; loop adv state
  | str -> 
    match parse str with
    | Quit -> quit ()
    | exception Empty -> print_string "Please enter a command.\n";
      print_shop adv state; shop_loop adv state
    | exception Malformed -> print_string "Not a valid command.\n";
      print_shop adv state; shop_loop adv state
    | Buy player ->
      let st = buy_helper player state in print_shop adv st; shop_loop adv st
    | Sell player ->
      let st = sell_helper player state in print_shop adv st; shop_loop adv st
    | _ -> print_string "You cannot do that heres."; shop_loop adv state

(** [arena_loop adv state] is the game loop on the arena screen. Reached by
    using commands [move arena] and [arena] from the main screen. Exited by
    typing "back". *)
and arena_loop adv state =
  print_string "\n> ";
  match read_line () with
  | "back" -> print_menu adv state; loop adv state
  | str ->
    match parse str with
    | Quit -> quit ()
    | exception Empty -> empty (); arena_loop adv state
    | exception Malformed -> malformed (); arena_loop adv state
    | Schedule team ->
      let st = challenge_helper team state in
      print_arena adv st; arena_loop adv st
    | Analysis team ->
      analysis_helper team adv; print_menu adv state; loop adv state
    | Team -> 
      print_current_team adv state; print_arena adv state; arena_loop adv state
    | Arena ->
      (match get_current_location state with 
       | Arena -> (print_arena adv state;
                   arena_loop adv state)
       | _ -> print_cmd_not_possible (Arena); loop adv state)
    | Update tactic -> 
      let st = update_helper tactic state in
      print_team adv st; arena_loop adv st
    | _ -> print_string "You cannot do that here."; arena_loop adv state

(** [team_loop adv state] is the game loop on the team screen. Reached by
    using the [team] command from any screen. Exited by typing "back". *)
and team_loop adv state =
  print_string "\n> ";
  match read_line () with
  | "back" -> print_menu adv state; loop adv state
  | str ->
    match parse str with 
    | Quit -> ignore(Sys.command "clear"); print_string "Bye!\n"; exit 0
    | exception Empty -> print_string "Please enter a command.\n";
      print_team adv state; team_loop adv state
    | exception Malformed -> print_string "Not a valid command.\n";
      print_team adv state; team_loop adv state
    | Scout player -> scout_helper player adv;
      print_team adv state; team_loop adv state
    | Active player ->
      let st = activate_helper player state in print_team adv st; team_loop adv st
    | Rest player ->
      let st = rest_helper player state in print_team adv st; team_loop adv st
    | _ -> print_string "You cannot do that here."; team_loop adv state

and train_loop adv state = 
  print_string "\n> ";
  match read_line () with
  | "back" -> print_menu adv state; loop adv state
  | str ->
    match parse str with
    | Quit -> ignore(Sys.command "clear"); print_string "Bye!\n"; exit 0
    | exception Empty -> print_string "Please enter a command.\n";
      print_train adv state; train_loop adv state
    | exception Malformed -> print_string "Not a valid command.\n";
      print_train adv state; train_loop adv state
    | Train player -> 
      let st = train_helper player state in
      print_train adv st; train_loop adv st
    | _ -> print_string "You cannot do that here."; 
      print_train adv state; train_loop adv state

(** The game loop on the main screen. *)
and loop adv state = 
  print_string "\n> ";
  match read_line () with
  | str -> 
    match parse str with 
    | Quit -> quit ()
    | exception Empty -> empty (); loop adv state
    | exception Malformed -> malformed (); loop adv state
    | Next_day -> (match get_current_location state with
        | Main ->
          let new_st = checkLegal (next_day state) in 
          print_next_day_info adv new_st; print_menu adv new_st; loop adv new_st
        | _ -> print_cmd_not_possible Next_day; loop adv state)
    | Schedule team -> (match get_current_location state with 
        | Arena -> print_string "To schedule a match, enter the arena by using \
                                 the 'arena' command."; loop adv state
        | _ -> print_cmd_not_possible (Schedule team); loop adv state)
    | Team -> print_team adv state; team_loop adv state
    | Arena -> (match get_current_location state with 
        | Arena -> (print_arena adv state; arena_loop adv state)
        | _ -> print_cmd_not_possible (Arena); loop adv state)
    | Shop -> (match get_current_location state with 
        | Shop -> (print_shop adv state; shop_loop adv state)
        | _ -> print_cmd_not_possible (Shop); shop_loop adv state)
    | Buy player | Sell player as cmd -> (match get_current_location state with
        | Shop ->
          print_string ("To " ^ (cmd_to_string cmd) ^ "a player, enter the shop \
                        by using the 'shop' command."); loop adv state
        | _ -> print_cmd_not_possible cmd; loop adv state)
    | Active _ | Rest _ ->
      print_string "To manage your team, try the 'team' command first.";
      loop adv state
    | Scout player -> scout_helper player adv;
      print_menu adv state; loop adv state
    | Analysis team -> analysis_helper team adv;
      print_menu adv state; loop adv state
    | Move location -> 
      (let location_string = String.concat " " location in
       match move state location with
       | Illegal -> 
         print_string ("Sorry, that is not a valid location! \
                         Try 'move main' to go to the home menu, \
                         'move shop' to go to the shop menu, \
                         'move arena' to go to the arena menu, \
                         and 'move training' to go to the training menu.\n");
         loop adv state
       | Legal(new_state) -> 
         print_string ("Moved to " ^ location_string ^ ".\n"); 
         print_menu adv new_state; loop adv new_state)
    | Help -> 
      print_help_command (get_current_location state); loop adv state
    | Practice -> (match get_current_location state with 
        | Training_Field -> 
          print_train adv state; train_loop adv state
        | _ -> 
          print_string "Command not possible in current location.\n \
                        Try 'move training' before doing the 'practice' \
                        command.\n"; loop adv state)
    | Train _ -> (match get_current_location state with
        | Training_Field -> 
          print_string "To train a player, enter the training field by using \
                        the 'practice' command."; train_loop adv state
        | _ -> 
          print_string "Command not possible in current location.\n Try 'move \
                        training' before doing the 'train' command.\n";
                        loop adv state)
    | Update tactic ->
      let st = update_helper tactic state in print_team adv st; loop adv st


let rec play_game ()=
  let file = read_line () in
  if (file = "quit")
  then begin ignore(Sys.command "clear"); print_string "Bye!\n"; exit 0 end 
  else match Yojson.Basic.from_file file with
    | exception End_of_file -> ()
    | exception _ ->
      print_string "Please enter a valid file name.\n> "; play_game ()
    | js -> begin
        ignore(Sys.command "clear");
        ANSITerminal.(print_string [red]
                        ("File succesfully loaded. You are now playing with " ^
                         file ^ ".\n"));
        flush stdout;
        let adv = from_json js in
        let initialState = init_state adv in 
        Unix.sleep 1;
        print_menu adv initialState;
        loop adv initialState
      end

let main () =
  print_title_screen ();
  play_game ()

(* Execute the game engine. *)
let () = main ()