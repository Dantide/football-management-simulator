open OUnit2
open Command
open Game
open State

(** Helper method to create an assert_equals test *)
let create_eq_test
    (name:string)
    (expected_output:'a)
    (output:'a) =
  name >:: (fun _ ->
      assert_equal expected_output output)

(** Helper method to create an assert_raises test *)
let create_exception_test
    (name:string)
    (expected_error:exn)
    (expression:exn) =
  name >:: (fun _ ->
      assert_raises expected_error (fun () -> expression))

(** Helper method to create a list of assert_equals tests. The name of the tests
    will be given by [func_name] and the first argument in each assoc stored in
    [arr]. The second argument will be the [input] of [func], and the third
    argument will be the [expected] output. *)
let create_eq_test_list
    (func_name: string)
    (func: 'a -> 'b)
    (arr: (string * 'a * 'b) list) =
  List.map (fun (case,input,expected) ->
      let str = Printf.sprintf "%s: %s" func_name case in
      str >:: (fun _ ->
          assert_equal expected (func input)))
    arr

(** Helper method to create a list of assert_raises tests. The name of the tests
    will be given by [func_name] and the first argument in each assoc stored in
    [arr]. The second argument will be the [input] of [func], and the third
    argument will be the [expected] error. *)
let create_exception_test_list
    (func_name: string)
    (func: 'a -> 'b)
    (arr: (string * 'a * exn) list) =
  List.map (fun (case,input,expected) ->
      let str = Printf.sprintf "%s: %s" func_name case in
      str >:: (fun _ ->
          assert_raises expected (fun () -> func input)))
    arr

(** Test command parsing for the Command module *)
let parse_tests = List.flatten [
    (* Parsing incorrect commands *)
    create_exception_test_list "parse" (parse)
      ["Empty", "", Empty;
       "Team Malformed", "team test", Malformed;
       "Shop Malformed", "shop test", Malformed;
       "Next_day Malformed", "nextday test", Malformed;
       "Quit Malformed", "quit test", Malformed;]
    ;

    (* Parsing with no extra values *)
    create_eq_test_list "parse" (parse)
      ["quit", "quit", Quit;
       "team", "team", Team;
       "shop", "shop", Shop;
       "next_day", "nextday", Next_day;]
    ;

    (* Parsing with one value *)
    create_eq_test_list "parse" (parse)
      ["schedule team", "schedule team", Schedule ["team"];
       "buy player", "buy player", Buy ["player"];
       "sell player", "sell player", Sell ["player"];
       "scout player", "scout player", Scout ["player"];]
    ;

    (* Parsing with two or more values *)
    create_eq_test_list "parse" (parse)
      ["schedule best team in league",
       "schedule best team in league", Schedule ["best";"team";"in";"league"];
       "buy michael jordan",
       "buy michael jordan", Buy ["michael";"jordan"];
       "sell My Star Player",
       "sell My Star Player", Sell ["My";"Star";"Player"];
       "scout That guy over ThEre",
       "scout That guy over ThEre", Scout ["That";"guy";"over";"ThEre"];]
    ;
  ]
let parse_tests = List.flatten [
    (* Parsing incorrect commands *)
    create_exception_test_list "parse" (parse)
      ["Empty", "", Empty;
       "Team Malformed", "team test", Malformed;
       "Shop Malformed", "shop test", Malformed;
       "Next_day Malformed", "nextday test", Malformed;
       "Quit Malformed", "quit test", Malformed;]
    ;

    (* Parsing with no extra values *)
    create_eq_test_list "parse" (parse)
      ["quit", "quit", Quit;
       "team", "team", Team;
       "shop", "shop", Shop;
       "next_day", "nextday", Next_day;
       "help", "help", Help;
       "practice", "practice", Practice]
    ;

    (* Parsing with one value *)
    create_eq_test_list "parse" (parse)
      ["buy player", "buy player", Buy ["player"];
       "sell player", "sell player", Sell ["player"];
       "scout player", "scout player", Scout ["player"];
       "challenge player", "challenge player", Schedule ["player"];
       "activate player", "activate player", Active ["player"];
       "rest player", "rest player", Rest ["player"];
       "analyze team", "analyze team", Analysis ["team"];
       "move place", "move place", Move ["place"];
       "train player", "train player", Train ["player"];
       "update tatics", "update tatics", Update ["tatics"]]
    ;

    (* Parsing with two or more values *)
    create_eq_test_list "parse" (parse)
      [
        "buy michael jordan",
        "buy michael jordan", Buy ["michael";"jordan"];
        "sell My Star Player",
        "sell My Star Player", Sell ["My";"Star";"Player"];
        "scout That guy over ThEre",
        "scout That guy over ThEre", Scout ["That";"guy";"over";"ThEre"];]
    ;
  ]

let adv = from_json (Yojson.Basic.from_file "data/test.json")
let st = init_state adv

let retrieve_buy_sell_info 
    (func: State.t -> Game.player_name -> State.result)
    (player_name: Game.player_name)
    (st: State.t)
  = match func st player_name with
  | Illegal -> get_current_shop st
  | Legal new_st -> get_current_shop new_st

(** Test buy player for the State module *)
let buy_tests =  [
  (* Parsing incorrect commands *)
  create_eq_test "test buy" ["Swen"; "Elroy"]
    (retrieve_buy_sell_info buy "" st);
  create_eq_test "test buy" ["Elroy"] 
    (retrieve_buy_sell_info buy "Swen" st);
  create_eq_test "test buy" ["Swen"] 
    (retrieve_buy_sell_info buy "Elroy" st);
]

(** Test sell player for the State module *)
let sell_tests = [
  create_eq_test "test sell" ["Swen"; "Elroy"]
    (retrieve_buy_sell_info sell "" st);
  create_eq_test "test sell" ["Titus"; "Swen"; "Elroy"]
    (retrieve_buy_sell_info sell "Titus" st);
  create_eq_test "test sell" ["Garfinkel"; "Swen"; "Elroy"]
    (retrieve_buy_sell_info sell "Garfinkel" st);
  create_eq_test "test sell" ["Churchill"; "Swen"; "Elroy"]
    (retrieve_buy_sell_info sell "Churchill" st);
  create_eq_test "test sell" ["Jesse"; "Swen"; "Elroy"]
    (retrieve_buy_sell_info sell "Jesse" st);
  create_eq_test "test sell" ["Bart"; "Swen"; "Elroy"]
    (retrieve_buy_sell_info sell "Bart" st);
]

(** Helper method to create a player  *)
let build_single_player 
    player_name 
    position 
    rating 
    current_team 
    price 
    player_id = {
  player_name = player_name;
  position = position;
  rating = rating;
  current_team = current_team;
  price = price;
  player_id = player_id;
  status = Rest;
  level = 0;
}


(** Test scout player for the State module *)
let scout_tests = [
  create_eq_test "test scout" 
    (Some (build_single_player "Bart" "Small Forward" {
         pace =  80;
         shooting = 79;
         defending = 70;
         passing= 78;
         dribbling =87;
         physicality=77;
         overall=82
       } "City Thunder" 399 11))
    (scout adv "Bart");

  create_eq_test "test scout" 
    None
    (scout adv "Ok");

  create_eq_test "test scout" 
    (Some (build_single_player "Titus" "Shooting Guard" {
         pace =  89;
         shooting = 86;
         defending = 76;
         passing= 81;
         dribbling =83;
         physicality=81;
         overall=86
       } "City Thunder" 421 12))
    (scout adv "Titus");
]


let suite =
  "test suite for Midterm Project" >::: List.flatten [
    parse_tests;
    buy_tests;
    sell_tests;
    scout_tests;
  ]

let _ = run_test_tt_main suite