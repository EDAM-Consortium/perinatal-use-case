#!/bin/bash
TOKEN="d2pat_AwRtUV7k2gORvirHwUfyEabjiSz7o7Jx2154674019"

# Step 1: Pull all metadata (fast — run once to browse available variables)
python fetch_fistula_metadata.py --token "$TOKEN"

# Step 2: Save a record of which IDs were selected for this run (every group)
grep -E '^\s+"[A-Za-z0-9]+",\s*#' fetch_fistula.py > fistula_selected_ids_log.txt
echo "Selected IDs logged to fistula_selected_ids_log.txt"

# Step 3: Pull data for selected variables (levels 2, 3, 4 only)
python fetch_fistula.py --token "$TOKEN"
