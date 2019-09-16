(* defined in module *)
type object_phrase = string list

(* defined in module *)
type command = 
  | Schedule of object_phrase
  | Team
  | Arena
  | Shop
  | Active of object_phrase
  | Rest of object_phrase
  | Buy of object_phrase
  | Sell of object_phrase
  | Scout of object_phrase
  | Analysis of object_phrase
  | Next_day
  | Quit
  | Move of object_phrase
  | Help
  | Train of object_phrase
  | Practice
  | Update of object_phrase

(* defined in module *)
exception Empty

(* defined in module *)
exception Malformed

(** [check_status str_lst] creates the command associated with [str_lst].

    Raises: [Malformed] if the command is [Quit], [Team], [Arena], [Shop], 
                [Next_day], [Practice], or [Help], and there is a trailing 
                object_phrase, or 
                if the command is [Schedule], [Active], [Rest], [Buy], [Sell], 
                [Scout], [Analysis], [Move], [Train], or [Update] and there is 
                no trailing object_phrase. *)
let check_status h t =
  match h with
  | "quit" when t = [] -> Quit
  | "team" when t = [] -> Team
  | "arena" when t = [] -> Arena
  | "shop" when t = [] -> Shop
  | "nextday" when t = [] -> Next_day
  | "practice" when t = [] -> Practice
  | "help" when t = [] -> Help

  | "challenge" when t != [] -> Schedule t
  | "activate" when t != [] -> Active t
  | "rest" when t != [] -> Rest t
  | "buy" when t != [] -> Buy t
  | "sell" when t != [] -> Sell t
  | "scout" when t != [] -> Scout t
  | "analyze" when t != [] -> Analysis t
  | "move" when t != [] -> Move t
  | "train" when t != [] -> Train t
  | "update" when t != [] -> Update t

  | _ -> raise Malformed

(** [check_empty lst] checks that [lst] is not the empty list, and therefore
    allow the process of creating a command to continue.

    Raises: [Empty] if [lst] is empty, aka an empty user input. *)
let check_empty = function
  | [] -> raise Empty
  | h :: t -> check_status h t

(** [deal str] is the list created from [str] after having been split by the 
    empty character. Empty string entries are removed.

    Example: [deal "Hello    world!"] would be [["Hello";"world!"]]. *)
let deal str = 
  String.split_on_char ' ' str |> 
  List.filter (fun s -> (String.length s) > 0)

(* defined in module *)
let parse str =
  str |> deal
  |> check_empty