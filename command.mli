(**
   Parsing of player commands.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["buy Michael Jordan"], then the object phrase 
      is [["Michael"; "Jordan"]].
    - If the player command is ["buy Michael    Jordan"], then the object 
      phrase is again [["Michael"; "Jordan"]].

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Schedule of object_phrase (* challenge the [object_phrase] team *)
  | Team (* call team list *)
  | Arena (* open arena menu, where user can see and challenge opponent teams *)
  | Shop (* open shop menu, where user can interact with the market *)
  | Active of object_phrase (* activate a player on your team *)
  | Rest of object_phrase (* rest a player on your team *)
  | Buy of object_phrase (* buy a player on the market *)
  | Sell of object_phrase (* sell a player to the market *)
  | Scout of object_phrase (* see a player's stats *)
  | Analysis of object_phrase (* analyze a team's stats and its players *)
  | Next_day (* updates caused by moving to the next day *)
  | Quit (* quit the game *)
  | Move of object_phrase (* move to menu location *)
  | Help (* displays all of the commands possible in the current location *)
  | Train of object_phrase (* train a player on your team *)
  | Practice (* open training menu, where user can train their players *)
  | Update of object_phrase (* update your team's play style *)

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    buy  Michael   Jordan   "] is [Buy ["Michael"; "Jordan"]]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb isn't "schedule", "team", "shop", "buy",
    "sell", "next day", "check", or "quit",
    or if the verb is "quit", "team", "shop", or "next day" and there is a 
    non-empty object phrase,
    or if the verb is "schedule", "buy", "sell", or "check" and there is an 
    empty object phrase.*)
val parse : string -> command
