import http.client
import json
import zlib
import pandas as pd

conn = http.client.HTTPSConnection("pinnacle-odds.p.rapidapi.com")

headers = {
    'x-rapidapi-key': "7a4c2e24e0msh255fc5705912e82p1e0f78jsna63590812381",  # Store this key securely
    'x-rapidapi-host': "pinnacle-odds.p.rapidapi.com",
    'Accept-Encoding': 'gzip'  # Request gzip encoding
}

# Send the GET request
conn.request("GET", "/kit/v1/special-markets?is_have_odds=true&sport_id=7", headers=headers)

# Get the response
res = conn.getresponse()
data = res.read()

# Check if the response is compressed (usually gzip)
if res.getheader('Content-Encoding') == 'gzip':
    data = zlib.decompress(data, zlib.MAX_WBITS | 16)  # Decompress gzip

# Decode and parse JSON response
try:
    json_data = json.loads(data.decode("utf-8"))
    print(json.dumps(json_data, indent=4))  # Pretty print the JSON response
except json.JSONDecodeError as e:
    print(f"Failed to parse JSON response: {e}")
except zlib.error as e:
    print(f"Failed to decompress data: {e}")

# Get anytime TD Markets------------------------------------------

# Empty list to append to
anytime_td_dfs = []

# Loop through and append to list
for event in json_data['specials']:
    if (event['category'] == "Player Props") and ("Anytime TD" in event.get('name', '')):
        name = event['name']
        home_team = event['event']['home']
        away_team = event['event']['away']
        anytime_td_df = pd.DataFrame(event['lines']).T
        anytime_td_df['selection'] = name
        anytime_td_df['home_team'] = home_team
        anytime_td_df['away_team'] = away_team
        anytime_td_dfs.append(anytime_td_df)

# Concatenate DataFrames
anytime_td_pinnacle = pd.concat(anytime_td_dfs, ignore_index=True)

# Write to csv
anytime_td_pinnacle.to_csv("OddsScraper/Pinnacle/pinnacle_tds_raw.csv")

# Get receptions Markets------------------------------------------

# Empty list to append to
receptions_dfs = []

# Loop through and append to list
for event in json_data['specials']:
    if (event['category'] == "Player Props") and ("Receptions" in event.get('name', '')):
        name = event['name']
        home_team = event['event']['home']
        away_team = event['event']['away']
        receptions_df = pd.DataFrame(event['lines']).T
        receptions_df['selection'] = name
        receptions_df['home_team'] = home_team
        receptions_df['away_team'] = away_team
        receptions_dfs.append(receptions_df)

# Concatenate DataFrames
receptions_pinnacle = pd.concat(receptions_dfs, ignore_index=True)

# Write to csv
receptions_pinnacle.to_csv("OddsScraper/Pinnacle/pinnacle_receptions_raw.csv")

# Get TD Passes Markets------------------------------------------

# Empty list to append to
td_passes_dfs = []

# Loop through and append to list
for event in json_data['specials']:
    if (event['category'] == "Player Props") and ("TD Passes" in event.get('name', '')):
        name = event['name']
        home_team = event['event']['home']
        away_team = event['event']['away']
        td_passes_df = pd.DataFrame(event['lines']).T
        td_passes_df['selection'] = name
        td_passes_df['home_team'] = home_team
        td_passes_df['away_team'] = away_team
        td_passes_dfs.append(td_passes_df)

# Concatenate DataFrames
td_passes_pinnacle = pd.concat(td_passes_dfs, ignore_index=True)

# Write to csv
td_passes_pinnacle.to_csv("OddsScraper/Pinnacle/pinnacle_td_passes_raw.csv")

# Get Passing Attempts Markets------------------------------------------

# Empty list to append to
passing_attempts_dfs = []

# Loop through and append to list
for event in json_data['specials']:
    if (event['category'] == "Player Props") and ("Pass Attempts" in event.get('name', '')):
        name = event['name']
        home_team = event['event']['home']
        away_team = event['event']['away']
        passing_attempts_df = pd.DataFrame(event['lines']).T
        passing_attempts_df['selection'] = name
        passing_attempts_df['home_team'] = home_team
        passing_attempts_df['away_team'] = away_team
        passing_attempts_dfs.append(passing_attempts_df)

# Concatenate DataFrames
passing_attempts_pinnacle = pd.concat(passing_attempts_dfs, ignore_index=True)

# Write to csv
passing_attempts_pinnacle.to_csv("OddsScraper/Pinnacle/pinnacle_passing_attempts_raw.csv")

# Get Passing Yards Markets------------------------------------------

# Empty list to append to
passing_yards_dfs = []

# Loop through and append to list
for event in json_data['specials']:
    if (event['category'] == "Player Props") and ("Passing Yards" in event.get('name', '')):
        name = event['name']
        home_team = event['event']['home']
        away_team = event['event']['away']
        passing_yards_df = pd.DataFrame(event['lines']).T
        passing_yards_df['selection'] = name
        passing_yards_df['home_team'] = home_team
        passing_yards_df['away_team'] = away_team
        passing_yards_dfs.append(passing_yards_df)

# Concatenate DataFrames
passing_yards_pinnacle = pd.concat(passing_yards_dfs, ignore_index=True)

# Write to csv
passing_yards_pinnacle.to_csv("OddsScraper/Pinnacle/pinnacle_passing_yards_raw.csv")

# Get Rushing Yards Markets------------------------------------------

# Empty list to append to
rushing_yards_dfs = []

# Loop through and append to list
for event in json_data['specials']:
    if (event['category'] == "Player Props") and ("Rushing Yards" in event.get('name', '')):
        name = event['name']
        home_team = event['event']['home']
        away_team = event['event']['away']
        rushing_yards_df = pd.DataFrame(event['lines']).T
        rushing_yards_df['selection'] = name
        rushing_yards_df['home_team'] = home_team
        rushing_yards_df['away_team'] = away_team
        rushing_yards_dfs.append(rushing_yards_df)

# Concatenate DataFrames
rushing_yards_pinnacle = pd.concat(rushing_yards_dfs, ignore_index=True)

# Write to csv
rushing_yards_pinnacle.to_csv("OddsScraper/Pinnacle/pinnacle_rushing_yards_raw.csv")

# Get Receiving Yards Markets------------------------------------------

# Empty list to append to
receiving_yards_dfs = []

# Loop through and append to list
for event in json_data['specials']:
    if (event['category'] == "Player Props") and ("Receiving Yards" in event.get('name', '')):
        name = event['name']
        home_team = event['event']['home']
        away_team = event['event']['away']
        receiving_yards_df = pd.DataFrame(event['lines']).T
        receiving_yards_df['selection'] = name
        receiving_yards_df['home_team'] = home_team
        receiving_yards_df['away_team'] = away_team
        receiving_yards_dfs.append(receiving_yards_df)

# Concatenate DataFrames
receiving_yards_pinnacle = pd.concat(receiving_yards_dfs, ignore_index=True)

# Write to csv
receiving_yards_pinnacle.to_csv("OddsScraper/Pinnacle/pinnacle_receiving_yards_raw.csv")
