#!/bin/bash
# Pull data for selected maternal/stillbirth indicators at Regional and Zonal level.
# Variables defined in SELECTED_IDS in fetch_maternal.py.
# Outputs: maternal_level2_Regional.csv, maternal_level3_Zonal.csv

python fetch_maternal.py --username Birhana --password Dhis2_12345 --levels 2 3
