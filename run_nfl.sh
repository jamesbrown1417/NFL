#!/bin/bash

# Exit on error, unset var, or failed pipe; print commands
set -euo pipefail

# Give access to normal path variables
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Configuration
PROJECT_DIR="/Users/jamesbrown/Projects/NFL"
PY="/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3"

# Set the current directory to your project folder
cd "$PROJECT_DIR" || exit 1

# Clean up stale artifacts (don't error if none found)
rm -f OddsScraper/Neds/*.json || true
rm -f OddsScraper/Pinnacle/*.csv || true
rm -f Data/scraped_odds/* || true

# Execute Python and R scripts (TAB, Pinnacle, Neds)
"$PY" OddsScraper/TAB/get-TAB-response.py
"$PY" OddsScraper/Pinnacle/scrape_pinnacle.py

"$PY" OddsScraper/Neds/get_neds_urls.py
Rscript OddsScraper/Neds/get_neds_match_urls.R
"$PY" OddsScraper/Neds/get_match_json.py

# Run master processing (sources Sportsbet, PointsBet, BetRight, Neds, Pinnacle tidy)
Rscript OddsScraper/master_processing_script.R

# Publish NFL reports using Quarto (auto-confirm)
echo "1" | quarto publish quarto-pub OddsScraper/NFL_Report.qmd
echo "1" | quarto publish quarto-pub OddsScraper/Top_Down.qmd

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage" || true # continue if no changes to commit

# Push the commit to the 'main' branch on 'origin'
git push origin main
