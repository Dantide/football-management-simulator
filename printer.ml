open Game
open State
open Command
open Sim

(* Helper Functions for Printing
 ******************************************************************************)

(** The number of columns in the currently opened terminal that the game is
    running in. *)
let width : int ref = ref 0


let quarter_of_int = function
  | 0 -> "1st Quarter"
  | 1 -> "2nd Quarter"
  | 2 -> "3rd Quarter"
  | 3 -> "4th Quarter"
  | _ -> raise (Failure "should not be happening")


let generic_player_headers = [
  "|     Name    "; "|| Price "; "||   Position     "; "|| ID # ";
  "|| Player Stats: "; "|| Pace "; "|| Shooting "; "|| Defending "; 
  "|| Passing "; "|| Dribbling "; "|| Physicality "; "|| Overall ";
]

let scout_player_headers = [
  "|     Name    "; "|| Price "; "||   Position     "; "|| ID # ";
  "|| Current Team "; "|| Player Stats: "; "|| Pace "; "|| Shooting "; 
  "|| Defending "; "|| Passing "; "|| Dribbling "; "|| Physicality "; 
  "|| Overall ";
]

let user_player_headers = [
  "|     Name    "; "|| Price "; "||   Position     "; "|| ID # ";
  "|| Status "; "|| Player Stats: "; "|| Pace "; "|| Shooting ";
  "|| Defending "; "|| Passing "; "|| Dribbling "; "|| Physicality "; 
  "|| Overall ";
]

let train_player_headers = [
  "|     Name    "; "||  Cost "; "|| Level "; "||   Position     "; "|| ID # ";
  "|| Status "; "|| Player Stats: "; "|| Pace "; "|| Shooting ";
  "|| Defending "; "|| Passing "; "|| Dribbling "; "|| Physicality "; 
  "|| Overall ";
]

let team_headers = [
  "|    Team Name    "; "||    Description    "; "|| ID # ";
  "|| Team Stats: "   ; "|| Attack " ; "|| Defence "; "|| Overall ";
]

let game_result_headers = [
  "|               "; "|| Your Score "; "|| Opponents Score";
]

let generic_player_array = List.map (fun p ->
    let r = p.rating in
    [p.player_name; p.price |> string_of_int; p.position;
     p.player_id |> string_of_int; ""; r.pace |> string_of_int;
     r.shooting |> string_of_int; r.defending |> string_of_int;
     r.passing |> string_of_int; r.dribbling |> string_of_int; 
     r.physicality |> string_of_int; r.overall |> string_of_int])

let scout_player_array = List.map (fun p ->
    let r = p.rating in
    [p.player_name; p.price |> string_of_int; p.position;
     p.player_id |> string_of_int; p.current_team; ""; r.pace |> string_of_int;
     r.shooting |> string_of_int; r.defending |> string_of_int; 
     r.passing |> string_of_int; r.dribbling |> string_of_int; 
     r.physicality |> string_of_int; r.overall |> string_of_int])

let user_active_player_array lst = List.map (fun p ->
    let r = p.rating in
    [p.player_name; p.price |> string_of_int; p.position;
     p.player_id |> string_of_int; "Active"; ""; r.pace |> string_of_int;
     r.shooting |> string_of_int; r.defending |> string_of_int; 
     r.passing |> string_of_int; r.dribbling |> string_of_int; 
     r.physicality |> string_of_int; r.overall |> string_of_int]) 
    (List.filter (fun p -> p.status = Active) lst)

let user_resting_player_array lst = List.map (fun p ->
    let r = p.rating in
    [p.player_name; p.price |> string_of_int; p.position;
     p.player_id |> string_of_int; "Rest"; ""; r.pace |> string_of_int;
     r.shooting |> string_of_int; r.defending |> string_of_int; 
     r.passing |> string_of_int; r.dribbling |> string_of_int; 
     r.physicality |> string_of_int; r.overall |> string_of_int]) 
    (List.filter (fun p -> p.status = Rest) lst)

let team_array = List.map (fun t ->
    let r = t.team_rating in
    [t.team_name; t.description; t.team_id |> string_of_int; "";
     r.attack |> string_of_int; r.defense |> string_of_int;
     r.overall |> string_of_int])

let train_array = List.map (fun p ->
    let r = p.rating in
    [p.player_name; (r.overall * 3) |> string_of_int; p.level |> string_of_int; 
     p.position; p.player_id |> string_of_int; 
     if p.status=Rest then "Rest" else "Active"; ""; 
     r.pace |> string_of_int; r.shooting |> string_of_int;
     r.defending |> string_of_int; r.passing |> string_of_int; 
     r.dribbling |> string_of_int; r.physicality |> string_of_int; 
     r.overall |> string_of_int])

let game_result_array result = List.mapi (fun i (our,opp) -> 
    [quarter_of_int i; our |> string_of_int; opp |> string_of_int])
    (List.combine result.our_score result.opp_score)



(** [wait_for_enter ()] waits for the user to press enter in stdin, until the
    enter key is pressed,  *)
(* Sourced from https://stackoverflow.com/questions/13410159/how-to-read-a-character-in-ocaml-without-a-return-key *)
let wait_for_enter () =
  print_string "-- PRESS ENTER --";
  flush stdout;
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false; Unix.c_echo = false; } in
  ignore (input_line stdin);
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  ignore(Sys.command "stty echo");
  ()


(** [print_table headers rows] prints a table with headers given by [headers]
    and rows given by [rows]. If a message should be printed at the top of the
    table, you can specify the message in the optional argument [msg].

    Requires: len([headers]) = len(elements of [rows]) *)
let print_table ?(msg:string option = None) (headers:string list)
    (rows:string list list) =
  (* Setup variables *)
  let length = String.length (List.fold_left (^) "" headers) in
  let headers_len = List.map (String.length) headers in
  (** [add_row init row] adds the row to init, but if any strings in [row] will
      not fit correctly in the column, it splits the string and creates a new
      row containing the parts that wouldn't fit. *)
  let rec add_row ?(rev:bool = true) init row = 
    (** [row_assoc] is an association list of [(str,l)], where str is the string
        in the row, and l is the max length of the where it fits in its column.
        To find l we take the length of the column's header and subtract 3
        (b/c we print ["|| " ^ str] and [String.length "|| " = 3]). *)
    let row_assoc = 
      List.map (fun (str,l) -> (str,l-3)) (List.combine row headers_len) in
    let all_short = 
      List.for_all (fun (str,l) -> String.length str < l) row_assoc in
    if all_short then row::init else 
      let first_row = List.map (fun (str,l) -> if str = "" then "" else
                                   String.sub str 0 (min (String.length str) l))
          row_assoc in
      let next_rows = List.map (fun (str,l) ->
          if String.length str < l
          then "" else String.sub str l (String.length str - l))
          row_assoc in
      if rev then List.rev (add_row ([first_row]) next_rows ~rev:false) @ init
      else add_row [first_row] next_rows ~rev:false @ init in 
  let split_rows = List.fold_left (add_row) [] (List.rev rows) in

  (* Print table header *)
  Format.open_tbox (); Format.set_tab ();
  let () = match msg with
    | None -> Format.print_tab ();
    | Some a -> Format.print_string a; Format.print_tab (); in
  Format.print_string ("|" ^ (String.make (length-1) '=') ^ "|");
  Format.print_tab ();
  List.iter (fun (x:string) -> Format.print_string x; Format.set_tab ())
    (headers);
  Format.print_string ("|\n|" ^ (String.make (length-1) '=') ^ "|");

  (* Print the table information from a list of entries *)
  let rec print_rows = function
    | [] -> ()
    | h::t ->
      Format.print_tab ();
      List.iteri (fun i str ->
          if i=0 then (Format.printf "| %s" str; Format.print_tab ())
          else (Format.printf "|| %s" str; Format.print_tab ())) h;
      Format.printf "|"; print_rows t; in
  print_rows split_rows;
  Format.print_string ("\n|" ^ (String.make (length-1) '=') ^ "|");
  Format.print_string "\n\n";
  Format.print_flush ();
  ()


(** [rebase_size ()] updates the width variable to the current width of the
    terminal. Done by storing the size in a text file, reading the file, then
    deleting it. *)
let rebase_size () =
  ignore(Sys.command "stty size > size.txt");
  let channel = open_in "size.txt" in
  width := int_of_string (List.nth (channel |> input_line
                                    |> String.split_on_char ' ') 1);
  close_in channel;
  ignore(Sys.command "rm -f \"size.txt\"")


(** Prints a row of stars exactly spanning the current terminal width. *)
let print_stars () =
  rebase_size ();
  print_endline (String.make !width '*')


(* Defined in module *)
let loc_to_string = function
  | Main -> "Main"
  | Shop -> "Shop"
  | Arena -> "Arena"
  | Training_Field -> "Training Field"

let cmd_to_string = function
  | Schedule _ -> "schedule"
  | Team -> "team"
  | Arena -> "arena"
  | Shop -> "shop"
  | Active _ -> "activate"
  | Rest _ -> "rest"
  | Buy _ -> "buy"
  | Sell _ -> "sell"
  | Scout _ -> "scout"
  | Analysis _ -> "analyze"
  | Next_day -> "next_day"
  | Quit -> "quit"
  | Move _ -> "move"
  | Help -> "help"
  | Train _ -> "train"
  | Practice -> "practice"
  | Update _ -> "update"

(* Ascii Graphics Printing
 ******************************************************************************)

(** Prints the graphic that shows when loading the game. *)
let print_start_screen_graphic () =
  (* start_screen_logo.txt has 25 lines of text *)
  ignore(Sys.command "stty -echo");
  let input = open_in "graphics/start_screen_logo.txt" in
  let print_lines num = 
    for i=1 to num do print_endline (input_line input) done in
  (* Print Basketball Man *)
  print_lines 5; Unix.sleepf 0.5;
  print_lines 7; Unix.sleepf 0.5;
  print_lines 7; Unix.sleepf 0.75;
  (* Print logo *)
  for i=1 to 6 do print_lines 1; Unix.sleepf 0.125 done;
  ignore(Sys.command "stty echo");
  close_in input

(** Prints the game name on the start screen. *)
let print_game_title () =
  ignore(Sys.command "stty -echo");
  (* game_title.txt has 12 lines of text *)
  let input = open_in "graphics/game_title.txt" in
  let print_lines num = for i=1 to num do
      ANSITerminal.(print_string [red] ((input_line input) ^ "\n")) done in
  print_lines 12;
  ignore(Sys.command "stty echo");
  close_in input


(* Main Printing Functions for Game Implementation
 ******************************************************************************)

(* Defined in module *)
let print_red = ANSITerminal.(print_string [red])

(* Defined in module *)
let print_blue = ANSITerminal.(print_string [blue])

(* Defined in module *)
let print_title_screen () =
  ignore(Sys.command "clear");
  print_start_screen_graphic ();
  Unix.sleepf 1.75;

  ignore(Sys.command "clear");
  print_stars ();
  print_game_title ();
  ANSITerminal.(print_string [red]
                  "\nWelcome to Fantasy Sports Managers.\n";);
  print_endline "Please enter the name of the game file you want to load.\n";
  print_endline "Note: small terminal dimensions may cause broken graphics. \
                 If you see broken graphics, please resize the window to be \
                 sufficiently wide.\n";
  print_stars ();
  print_string  "\n> "


(* Defined in module *)
let print_cmd_not_possible cmd =
  let general =
    "Command not possible in current location.\n "
  in match cmd with
  | Shop | Sell _ | Buy _ -> 
    print_string (general ^ "Try 'move shop' before doing the '" ^
                  cmd_to_string cmd ^ "' command.\n")
  | Arena | Schedule _ | Update _ ->
    print_string (general ^ "Try 'move arena' before doing the '" ^
                  cmd_to_string cmd ^ "' command.\n")
  | Next_day ->
    print_string (general ^ "Try 'move main' before doing the '" ^
                  cmd_to_string cmd ^ "' command.\n")
  | Train _ | Practice ->
    print_string (general ^ "Try 'move arena' before doing the '" ^
                  cmd_to_string cmd ^ "' command.\n")
  | Active _ | Rest _ ->
    print_string (general 
                  ^ "To manage your team, try the 'team' command \
                     before doing the '" ^ cmd_to_string cmd 
                  ^ "' command.\n")
  | Quit | Help | Team | Scout _ | Analysis _ | Move _ -> print_string general



(** Used in print_menu to display the commands that a player may type to play
    the game. *)
let print_available_commands =
  let general =
    "Commands (type help for more information):\
     \n     General: team, quit, help, scout, analyze, move"
  in function
    | Main -> ANSITerminal.(
        print_string [blue] (general ^ " [shop, arena, training]\
                                        \n     Location specific: nextday.\n"))
    | Shop -> ANSITerminal.(
        print_string [blue] (general ^ " [main, arena, training]\
                                        \n     Location specific: shop.\n"))
    | Arena -> ANSITerminal.(
        print_string [blue] (general ^ " [main, shop, training]\
                                        \n     Location specific: arena, \
                                        update.\n"))
    | Training_Field -> ANSITerminal.(
        print_string [blue] (general ^ " [main, shop, arena]\
                                        \n     Location specific: practice.\n"))



(* Defined in module *)
let print_help_command =
  let general = "Possible commands:\n \
                 - team : Display your team information and manage your active \
                 players.\n \
                 - quit : Quit the game.\n \
                 - help : Display the possible commands.\n \
                 - scout [playername] : Display the information \
                 about [playername].\n \
                 - analyze [teamname] : Display the information \
                 about [teamname].\n \
                 - move [location] : Move to [location] menu.\n "
  in function
    | Main -> 
      print_string (general ^ "- nextday : Move on to the next day, \
                               update your information.\n")
    | Shop -> 
      print_string (general ^ "- shop : Enter the player shop.\n")
    | Arena -> 
      print_string (general ^ "- arena : Enter the game arena.\n")
    | Training_Field -> 
      print_string (general ^ "- practice : Enter the training field.\n")


(* Defined in module *)
let print_basic_info adv state = 
  let team_name = match get_single_team adv (get_your_team adv) with
    | None -> failwith "impossible"
    | Some h -> h.team_name in 
  let current_date = state |> get_current_date |> string_of_int in 
  let current_money = state |> get_current_money |> string_of_int in
  let name_str = List.fold_left (fun acc elt -> elt.player_name ^ ", " ^ acc) 
      "" (get_current_active_players state) in
  print_endline ("Day " ^ current_date ^ ":");
  print_endline ("Hello " ^ team_name ^ "\'s manager!");
  print_endline ("You have " ^ current_money ^ " dollars.");
  if List.length (get_current_active_players state) = 0 
  then print_string "Nobody"
  else 
    print_string (String.sub name_str 0 (String.length name_str - 2));
  print_string " are ready to play!\n"


(* Defined in module *)
let print_next_day_info adv state = 
  let current_date = (get_current_date state) in 
  let current_money = (get_current_money state) in 
  print_string "\nDay ";
  print_int current_date;
  print_string ":\n";
  print_string "Good morning! Now You have ";
  print_int current_money;
  print_string " dollars. And your energy is full again! 
  What are you gonna do today? \n";
  print_string "But remember, your enemies get stronger every 7 days!\n"


(* Defined in module *)
let print_menu adv state =
  ignore(Sys.command "clear");
  print_stars ();
  print_endline ("current location: " ^
                 loc_to_string (get_current_location state));
  print_endline ("current energy: " ^
                 (get_current_energy state |> string_of_int) ^ "\n");
  print_basic_info adv state;
  print_newline ();
  print_available_commands (get_current_location state);
  print_stars ()


(* Defined in module *)
let print_game result =
  ignore(Sys.command "clear");
  print_table game_result_headers (game_result_array result);
  begin
    if (List.nth result.opp_score 3 > List.nth result.our_score 3) then
      print_string "You Lose!\n" else
    if (List.nth result.opp_score 3 > List.nth result.our_score 3) then
      print_string "It's a Tie!\n"
    else print_string "You Win!\n"
  end;
  wait_for_enter ()


(* Table printing functions
 ******************************************************************************)

(* Defined in module, consider deleting *)
let print_current_team adv state =
  ignore(Sys.command "clear");
  let player_headers = generic_player_headers in
  let player_array = generic_player_array (get_current_team state) in
  print_table player_headers player_array
    ~msg: (Some "Here is the information about your current Team!");
  wait_for_enter ()



(** Used in Scout *)
(* Defined in module *)
let rec print_single_player player = 
  ignore(Sys.command ("clear"));
  let headers = scout_player_headers in
  let array = (scout_player_array [player]) in
  print_table headers array;
  wait_for_enter ()



(** Used in Analyze *)
(* Defined in module *)
let rec print_single_team team = 
  ignore(Sys.command ("clear"));
  (* Print team information -- OUTDATED *)
  print_table team_headers (team_array [team]);
  (* let counter = count_stats_num team.players in
     print_string ("There are " ^ (string_of_int (fst counter)) 
                ^ " forward(s), and " ^ (string_of_int (snd counter)) 
                ^ " defender(s) activated in " ^ team.team_name ^ "!\n");  *)
  let new_counter = count_stats_ability team.players in 
  (* let new_str = "Offensive ability: " ^ (string_of_int (fst new_counter)) 
                ^ ".\n" ^ "Defensive ability: " 
                ^ (string_of_int (snd new_counter))^".\n" in
     print_string new_str;  *)
  let msg = team.team_name ^ " is a " 
            ^ (if fst new_counter < snd new_counter
               then "defensive-oriented team!\n" 
               else "offensive-oriented team!\n") in 
  print_string msg;

  let player_headers = generic_player_headers in
  let player_array = generic_player_array team.players in
  print_table player_headers player_array;
  wait_for_enter ();
  ()

(* Defined in module *)
let print_shop adv state =
  ignore(Sys.command ("clear"));
  let headers = generic_player_headers in
  let array = generic_player_array (get_current_market state) in
  print_table headers array
    ~msg:(Some "Welcome to the Shop! Here is a list of all the free agents \
                available for purchase!");
  ANSITerminal.(print_string [blue]
                  "To buy someone, type 'buy playername' where playername is \
                   the name of the player you want to buy.\n\
                   To sell someone, type 'sell playername' where playername \
                   is the name of the player you want to sell.\n\
                   To back out of the shop, type 'back'.\n")


(* TODO Implement *)
let print_team adv state =
  ignore(Sys.command "clear");
  (* Print team stats -- OUTDATED *)
  (* let counter = count_stats_num (get_current_active_players state) in
     print_string ("There are " ^ (string_of_int (fst counter)) 
                ^ " forward(s), and " ^ (string_of_int (snd counter)) 
                ^ " defender(s) activated!\n"); 
     let new_counter = count_stats_ability (get_current_active_players state) in
     print_endline("Offensive ability: " ^ (string_of_int (fst new_counter)) 
                ^ ".\nDefensive ability: " ^ (string_of_int (snd new_counter)) 
                ^ ".\n"); *)
  print_endline "Here is the information about your current team!\n";
  (* Print team lists *)
  let headers = user_player_headers in
  let active_array = user_active_player_array (get_current_team state) in
  let rest_array = user_resting_player_array (get_current_team state) in
  print_table headers active_array ~msg: (Some "Active players:");
  print_table headers rest_array ~msg: (Some "Resting players:");
  ANSITerminal.(print_string [blue]
                  "To activate a player, type 'activate playername' where \
                   playername is the name of the player you want to set on \
                   your active roster.\n\
                   To rest a player, type 'rest playername' where \
                   playername is the name of the player you want to remove \
                   from your active roster.\n\
                   To leave the team menu, type 'back'.\n")


(* Defined in module *)
let print_arena adv state =
  ignore(Sys.command ("clear"));
  let headers = team_headers in
  let array = team_array (get_current_opponent_teams state) in
  print_table headers array
    ~msg:(Some "Welcome to the Arena! Here is a list of all the opponent teams \
                available for challenge!");
  ANSITerminal.(print_string [blue]
                  "To schedule a match against another team, type 'challenge \
                   teamname' where teamname is the name of the team you want \
                   to play against.\n\
                   To back out of the arena, type 'back'.\n")


(* defined in module *)
let print_train adv state =
  ignore(Sys.command ("clear"));
  let headers = train_player_headers in 
  let array = train_array (get_current_team state) in 
  print_table headers array
    ~msg:(Some "Welcome to the Training Field! Here is a list of the players \
                on your team!");
  ANSITerminal.(print_string [blue]
                  "To train someone, type 'train playername' where playername \
                   is the name of the player you want to train.\n\
                   to exit the training field, type 'back'.\n")
