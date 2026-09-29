"""
Pulls metadata (names, IDs, dataset info) for all obstetric fistula and
prolapse data elements and indicators from the Ethiopian MoH DHIS2 instance,
then probes each one against the analytics API to confirm whether data
actually exists on the server.

Saves to fistula_metadata.csv — run this first to browse all available
variables before selecting a subset for fetch_fistula.py.
Only variables with has_data=True are worth including in fetch_fistula.py.

Usage:
    python fetch_fistula_metadata.py --username X --password Y
"""

import sys
import csv
import time
import argparse
import requests
import urllib3
from pathlib import Path
from requests.adapters import HTTPAdapter

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

BASE = "https://dhis.moh.gov.et/api"

parser = argparse.ArgumentParser()
parser.add_argument("--token", "-t", required=True,
                    help="DHIS2 Personal Access Token (create in profile → Edit profile → Personal access tokens)")
args = parser.parse_args()

session = requests.Session()
session.headers.update({
    "Accept": "application/json",
    "Authorization": f"ApiToken {args.token}",
})
session.verify = False

class SSLIgnoreAdapter(HTTPAdapter):
    def send(self, *args, **kwargs):
        kwargs["verify"] = False
        return super().send(*args, **kwargs)

session.mount("http://", SSLIgnoreAdapter())
session.mount("https://", SSLIgnoreAdapter())


def get(path, params=None):
    for attempt in range(1, 4):
        try:
            r = session.get(f"{BASE}{path}", params=params, timeout=180)
            if r.status_code == 401:
                sys.exit("Authentication failed.")
            r.raise_for_status()
            return r.json()
        except Exception as e:
            if attempt == 3:
                sys.exit(f"Connection failed after 3 attempts: {e}")
            print(f"Connection attempt {attempt} failed, retrying in 10s...")
            time.sleep(10)

# Sample period: 2013–2019 at zonal level — broad enough to catch most variables
PROBE_PERIODS = ";".join(
    f"{y}{m:02d}" for y in range(2013, 2020) for m in range(1, 13)
)
PROBE_LEVEL = 3  # Zonal

def probe_data(dx_id):
    """Return row count from analytics for a sample period, or -1 on 409 (derived indicator)."""
    try:
        r = session.get(f"{BASE}/analytics", params={
            "dimension": [
                f"dx:{dx_id}",
                f"pe:{PROBE_PERIODS}",
                f"ou:LEVEL-{PROBE_LEVEL}",
                "co",
            ],
            "skipMeta": "true",
            "paging": "false",
        }, timeout=120)
        if r.status_code == 409:
            return -1
        r.raise_for_status()
        return len(r.json().get("rows", []))
    except Exception:
        return 0

SEARCH_TERMS = [
    "fistula",
    "obstetric fistula",
    "prolapse",
    "cord prolapse",
    "uterine prolapse",
    "vesico",
    "rectovaginal",
    "vesicovaginal",
]

out_path = Path("./fistula_metadata.csv")

if out_path.exists():
    print(f"\nfistula_metadata.csv already exists — skipping metadata pull.")
    print(f"Delete fistula_metadata.csv and re-run to refresh.")
    sys.exit(0)

print("\nConnecting...")
info = get("/system/info")
print(f"System: {info.get('systemName')}  v{info.get('version')}\n")
print("Fetching fistula and prolapse metadata...")

rows = []
seen_de, seen_ind = set(), set()

for term in SEARCH_TERMS:
    print(f"  Searching: {term}")

    de_r = get("/dataElements", {
        "filter": f"name:ilike:{term}",
        "fields": "id,name,shortName,valueType,domainType,dataSets[name,periodType,openingDate]",
        "paging": "false",
    })
    for de in de_r.get("dataElements", []):
        if de["id"] not in seen_de:
            seen_de.add(de["id"])
            datasets = de.get("dataSets", [])
            rows.append({
                "type": "data_element",
                "id": de["id"],
                "name": de["name"],
                "short_name": de.get("shortName", ""),
                "value_type": de.get("valueType", ""),
                "domain_type": de.get("domainType", ""),
                "dataset_names": " | ".join(ds["name"] for ds in datasets),
                "period_types": " | ".join(ds.get("periodType", "") for ds in datasets),
                "opening_dates": " | ".join(ds.get("openingDate", "") for ds in datasets),
                "search_term": term,
                "data_rows_sample": None,
                "has_data": None,
            })

    ind_r = get("/indicators", {
        "filter": f"name:ilike:{term}",
        "fields": "id,name,shortName,indicatorType[name]",
        "paging": "false",
    })
    for ind in ind_r.get("indicators", []):
        if ind["id"] not in seen_ind:
            seen_ind.add(ind["id"])
            rows.append({
                "type": "indicator",
                "id": ind["id"],
                "name": ind["name"],
                "short_name": ind.get("shortName", ""),
                "value_type": ind.get("indicatorType", {}).get("name", ""),
                "domain_type": "",
                "dataset_names": "",
                "period_types": "",
                "opening_dates": "",
                "search_term": term,
                "data_rows_sample": None,
                "has_data": None,
            })

print(f"\nFound {len(rows)} variables ({sum(1 for r in rows if r['type'] == 'data_element')} data elements, "
      f"{sum(1 for r in rows if r['type'] == 'indicator')} indicators)")
print(f"\nProbing analytics API for each variable (2013–2019, zonal level)...")

for i, row in enumerate(rows, 1):
    n = probe_data(row["id"])
    if n == -1:
        row["data_rows_sample"] = 0
        row["has_data"] = "derived_indicator_not_queryable"
    else:
        row["data_rows_sample"] = n
        row["has_data"] = "TRUE" if n > 0 else "FALSE"
    status = row["has_data"] if n <= 0 else f"TRUE ({n} rows)"
    print(f"  [{i:>3}/{len(rows)}]  {row['id']}  {status}  {row['name'][:60]}")
    time.sleep(1)

n_with_data = sum(1 for r in rows if r["has_data"] == "TRUE")
print(f"\n{n_with_data}/{len(rows)} variables have data on the server.")

with open(out_path, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=[
        "type", "id", "name", "short_name", "value_type",
        "domain_type", "dataset_names", "period_types",
        "opening_dates", "search_term", "data_rows_sample", "has_data",
    ])
    writer.writeheader()
    writer.writerows(rows)

print(f"Saved {len(rows)} rows to {out_path}")
print(f"\nFilter has_data=TRUE in fistula_metadata.csv before selecting variables.")
