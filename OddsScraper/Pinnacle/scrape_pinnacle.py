import http.client
import json
import os
import zlib
import pandas as pd

conn = http.client.HTTPSConnection("pinnacle-odds.p.rapidapi.com")

# Prefer env var key if available
api_key = os.getenv("PINNACLE_RAPIDAPI_KEY", "7a4c2e24e0msh255fc5705912e82p1e0f78jsna63590812381")

headers = {
    'x-rapidapi-key': api_key,  # Consider setting PINNACLE_RAPIDAPI_KEY in env
    'x-rapidapi-host': "pinnacle-odds.p.rapidapi.com",
    'Accept-Encoding': 'gzip'  # Request gzip encoding
}

# Send the GET request for NFL (sport_id=7)
conn.request("GET", "/kit/v1/special-markets?is_have_odds=true&sport_id=7", headers=headers)

# Get the response
res = conn.getresponse()
data = res.read()

# Decompress if response is gzip encoded
if res.getheader('Content-Encoding') == 'gzip':
    data = zlib.decompress(data, zlib.MAX_WBITS | 16)

# Decode and parse JSON response
try:
    json_data = json.loads(data.decode("utf-8"))
except (json.JSONDecodeError, zlib.error) as e:
    print(f"Error processing data: {e}")
    raise SystemExit(1)


def extract_market_data(json_data, market_filters, file_name, use_line_keys_for_selection=False):
    """
    Extracts markets where any of the strings in market_filters is contained
    in the event name (case-insensitive), restricted to Player Props.
    market_filters can be a string or a list of strings.
    """
    if isinstance(market_filters, str):
        market_filters = [market_filters]
    market_filters_lc = [mf.lower() for mf in market_filters]

    market_dfs = []

    for event in json_data.get('specials', []):
        if event.get('category') != "Player Props":
            continue
        event_name = event.get('name', '')
        event_name_lc = event_name.lower()
        if not any(mf in event_name_lc for mf in market_filters_lc):
            continue

        home_team = event['event']['home']
        away_team = event['event']['away']

        # Extract player name before " - ", fallback to full name
        player_name_extracted = event_name.split(' - ')[0] if ' - ' in event_name else event_name

        market_df = pd.DataFrame(event['lines']).T
        # Keep selection as provided by API (e.g., "Dak Prescott Total Passing Yards")
        market_df['selection'] = event_name
        market_df['player_name'] = player_name_extracted
        market_df['home_team'] = home_team
        market_df['away_team'] = away_team
        market_dfs.append(market_df)

    out_path = f"OddsScraper/Pinnacle/{file_name}.csv"
    if market_dfs:
        final_df = pd.concat(market_dfs, ignore_index=True)
        final_df.to_csv(out_path, index=False)
        print(f"[Pinnacle] Saved {out_path} ({' | '.join(market_filters)})")
        return len(final_df)
    else:
        print(f"[Pinnacle] No data found for filter(s): {' | '.join(market_filters)}")
        return 0




# Extract all NFL markets we tidy downstream
# More flexible aliases to match Pinnacle naming variants
td_rows = extract_market_data(json_data, [
    "anytime td",
    "any time td",
    "anytime touchdown",
    "any time touchdown",
    "anytime touchdown scorer",
], "pinnacle_tds_raw", use_line_keys_for_selection=False)

extract_market_data(json_data, "Receptions", "pinnacle_receptions_raw")

extract_market_data(json_data, [
    "td passes",
    "touchdown passes",
    "passing tds",
    "pass tds",
], "pinnacle_td_passes_raw")

extract_market_data(json_data, "Pass Attempts", "pinnacle_passing_attempts_raw")
extract_market_data(json_data, "Passing Yards", "pinnacle_passing_yards_raw")
extract_market_data(json_data, "Rushing Yards", "pinnacle_rushing_yards_raw")
extract_market_data(json_data, "Receiving Yards", "pinnacle_receiving_yards_raw")

conn.close()
print("NFL player prop data extraction completed.")
