import json
import numpy as np

player_id = 1
team_id = 1
event_id = 1
date = 1
positions = ["Small Forward", "Shooting Guard",
             "Power Forward", "Point Guard", "Center"]
play_styles = ["ultra_defensive", "defensive", "balanced", "attacking",
               "ultra_attacking"]


def get_player_names():
    with open("male_names.txt", "r") as f:
        a = f.read()
    return a.split("\n")


def create_players(n, player_names, team_name):
    global player_id, positions
    player_list = []
    for i in range(n):
        player = {}
        player["player name"] = player_names.pop(
            np.random.randint(0, len(player_names)))
        if n == 5:
            player['position'] = positions[i]
        else:
            player['position'] = positions[np.random.randint(len(positions))]
        player["rating"] = create_player_rating(player["position"])
        player["current team"] = team_name
        player['price'] = 175 + (player["rating"]["overall"]**2)//30
        player["player id"] = player_id
        player_id += 1
        player_list.append(player)
    return player_list


def create_player_rating(position):
    lower_limit = np.random.randint(65, 83)
    rating = {}
    if position == "Small Forward":
        rating["pace"] = np.random.randint(lower_limit, 90)
        rating["shooting"] = np.random.randint(lower_limit, 88)
        rating["defending"] = np.random.randint(min(lower_limit, 70), 77)
        rating["passing"] = np.random.randint(72, 85)
        rating["dribbling"] = np.random.randint(lower_limit, 89)
        rating["physicality"] = np.random.randint(65, 83)
    elif position == "Shooting Guard":
        rating["pace"] = np.random.randint(75, 90)
        rating["shooting"] = np.random.randint(lower_limit, 90)
        rating["defending"] = np.random.randint(73, 90)
        rating["passing"] = np.random.randint(69, 84)
        rating["dribbling"] = np.random.randint(lower_limit, 90)
        rating["physicality"] = np.random.randint(78, 90)
    elif position == "Power Forward":
        rating["pace"] = np.random.randint(70, 87)
        rating["shooting"] = np.random.randint(70, 90)
        rating["defending"] = np.random.randint(72, 83)
        rating["passing"] = np.random.randint(lower_limit, 90)
        rating["dribbling"] = np.random.randint(lower_limit, 90)
        rating["physicality"] = np.random.randint(68, 90)
    elif position == "Point Guard":
        rating["pace"] = np.random.randint(lower_limit, 90)
        rating["shooting"] = np.random.randint(lower_limit, 87)
        rating["defending"] = np.random.randint(lower_limit, 90)
        rating["passing"] = np.random.randint(lower_limit + 4, 90)
        rating["dribbling"] = np.random.randint(lower_limit + 3, 90)
        rating["physicality"] = np.random.randint(lower_limit, 86)
    else:
        rating["pace"] = np.random.randint(lower_limit, 87)
        rating["shooting"] = np.random.randint(lower_limit, 87)
        rating["defending"] = np.random.randint(lower_limit + 5, 90)
        rating["passing"] = np.random.randint(lower_limit, 89)
        rating["dribbling"] = np.random.randint(lower_limit-2, 86)
        rating["physicality"] = np.random.randint(max(80, lower_limit), 90)
    values = list(rating.values())
    values.sort()
    rating["overall"] = list_sum(values[3:])//3
    return rating


def create_teams(player_names):
    global team_id, play_styles
    team_list = ["Raptors", "Knicks", "City Thunder", "Lakers", "Warriors", "Spurs", "Bulls",
                 "Suns", "Bucks", "Rockets", "Mavericks", "Cavaliers", "Jazz", "Celtics",
                 "Heat"]
    teams = []
    for team_name in team_list:
        team = {}
        player_list = create_players(5, player_names, team_name)
        team_rating = calculate_team_rating(player_list)
        team["team name"] = team_name
        team["description"] = "Team " + team_name
        team["play style"] = play_styles[np.random.randint(
            0, len(play_styles))]
        team["team rating"] = team_rating
        team["players"] = player_list
        team["team id"] = team_id
        team_id += 1
        teams.append(team)
    return teams


def list_sum(lst):
    total = 0
    for i in lst:
        total += i
    return total


def calculate_team_rating(player_list):
    pace = []
    shooting = []
    defending = []
    passing = []
    dribbling = []
    physicality = []
    for player in player_list:
        rating = player['rating']
        pace.append(rating['pace'])
        shooting.append(rating['shooting'])
        passing.append(rating['passing'])
        dribbling.append(rating['dribbling'])
        physicality.append(rating['physicality'])
        defending.append(rating["defending"])
    pace.sort()
    shooting.sort()
    defending.sort()
    passing.sort()
    dribbling.sort()
    physicality.sort()
    overall = 0
    for player in player_list:
        rating = player['rating']
        overall += rating['overall']
    chemistry = np.random.randint(65, 90)
    attack_rating = (list_sum(shooting) +
                     list_sum(passing + dribbling + pace))//20
    defense_rating = (list_sum(defending) + list_sum(physicality))//10

    overall_rating = overall//5

    return {"attack": attack_rating, "defense": defense_rating,
            "chemistry": chemistry, "overall": overall_rating}


def create_special_events():
    global event_id
    special_events_names = ["friendly match", "champions league game", "charity game",
                            "promotional game"]
    dates = [5, 21, 34, 36, 53]
    special_events = []
    for event_name in special_events_names:
        event = {}
        event["event name"] = event_name
        event["event reward"] = np.random.randint(3, 8)*100
        event["event date"] = dates.pop(0)
        event["event id"] = event_id
        event_id += 1
        special_events.append(event)
    return special_events


if __name__ == "__main__":
    main = {}
    budget = 2000
    your_team = 3
    player_names = get_player_names()
    teams = create_teams(player_names)
    main["teams"] = teams
    market = create_players(25, player_names, "")
    main["market"] = market
    special_events = create_special_events()
    main["special events"] = special_events
    main["budget"] = budget
    main["date"] = 1
    main["your team"] = your_team
    with open("game.json", 'w') as j:
        j.write(json.dumps(main, indent=4))
