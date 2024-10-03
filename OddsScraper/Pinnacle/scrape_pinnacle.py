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