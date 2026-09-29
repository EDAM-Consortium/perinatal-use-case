"""
Pulls obstetric fistula and prolapse data from the Ethiopian MoH DHIS2 instance.
Collects data at admin levels 2 (Regional), 3 (Zonal), and 4 (Wereda) only.
Saves a separate CSV per level, with adaptive chunk sizes.
Re-run safe: skips periods already queried (whether or not they returned data),
tracked per named group in SELECTED_GROUPS so new groups can be added later
without re-fetching or duplicating data already pulled for existing groups.

Step 1: Run fetch_fistula_metadata.py to browse all available variables.
Step 2: Copy IDs of interest into a group in SELECTED_GROUPS below
        (new selections go in their own new group — see comments there).
Step 3: Run this script.

Usage:
    python fetch_fistula.py --username X --password Y
    python fetch_fistula.py --username X --password Y --chunk-override 3
"""

import sys
import csv
import time
import argparse
import requests
import urllib3
from pathlib import Path
from datetime import date
from requests.adapters import HTTPAdapter

urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

BASE = "https://dhis.moh.gov.et/api"

parser = argparse.ArgumentParser()
parser.add_argument("--token", "-t", required=True,
                    help="DHIS2 Personal Access Token (create in profile → Edit profile → Personal access tokens)")
parser.add_argument("--chunk-override", type=int, default=None,
                    help="Override chunk size for all levels (e.g. 3, 6, 12). "
                         "Increase on re-runs to fill gaps more efficiently.")
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


# Only levels 2–4; levels 1, 5, 6 are intentionally excluded
LEVELS_TO_RUN = {2, 3, 4}

CHUNK_BY_LEVEL = {
    2: 12,
    3: 6,
    4: 3,
}
DEFAULT_CHUNK = 3

MAX_RETRIES = 4
PAUSE_BETWEEN_CHUNKS = 3

CSV_COLS = [
    "data_element_id", "data_element_name",
    "category_option_combo_id", "category_option_combo_name",
    "period", "period_type",
    "org_unit_level", "org_unit_level_name",
    "org_unit_id", "org_unit_name", "org_unit_hierarchy",
    "value",
]

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

def fetch_chunk_with_retry(params, chunk_label):
    for attempt in range(1, MAX_RETRIES + 1):
        try:
            r = session.get(f"{BASE}/analytics", params=params, timeout=300)
            r.raise_for_status()
            return r.json()
        except (requests.Timeout, requests.ConnectionError) as e:
            if attempt == MAX_RETRIES:
                print(f"    {chunk_label}: giving up after {MAX_RETRIES} attempts ({e})")
                return None
            wait = 15 * attempt
            print(f"    {chunk_label}: timeout (attempt {attempt}/{MAX_RETRIES}), retrying in {wait}s...")
            time.sleep(wait)
        except Exception as e:
            print(f"    {chunk_label}: error {e}")
            return None

def get_queried_periods(log_path):
    queried = set()
    if log_path.exists():
        with open(log_path, "r", encoding="utf-8") as f:
            for line in f:
                p = line.strip()
                if p:
                    queried.add(p)
        print(f"  Found queried-periods log with {len(queried)} periods — skipping those.")
    return queried

def log_queried_periods(log_path, periods):
    with open(log_path, "a", encoding="utf-8") as f:
        for p in periods:
            f.write(p + "\n")

def fetch_level(dx_ids, group_name, period_type, ou_level, ou_level_name, all_periods, csv_path):
    chunk_size = args.chunk_override if args.chunk_override else CHUNK_BY_LEVEL.get(ou_level, DEFAULT_CHUNK)
    log_path = Path("queries") / f"{csv_path.stem}__{group_name}.queried"

    queried_periods = get_queried_periods(log_path)

    chunks = [all_periods[i:i+chunk_size] for i in range(0, len(all_periods), chunk_size)]
    chunks_to_run = [c for c in chunks if not all(p in queried_periods for p in c)]

    skipped = len(chunks) - len(chunks_to_run)
    print(f"  Chunk size: {chunk_size}  |  Total chunks: {len(chunks)}  |  "
          f"Skipping {skipped} already queried  |  Running {len(chunks_to_run)}")

    if not chunks_to_run:
        print(f"  Level {ou_level} already complete — nothing to do.")
        return 0

    write_header = not csv_path.exists()
    csv_file = open(csv_path, "a", newline="", encoding="utf-8")
    csv_writer = csv.DictWriter(csv_file, fieldnames=CSV_COLS)
    if write_header:
        csv_writer.writeheader()
    csv_file.flush()

    dx_str = ";".join(dx_ids)
    ou_str = f"LEVEL-{ou_level}"
    total_for_level = 0

    for i, chunk in enumerate(chunks_to_run, 1):
        pe_str = ";".join(chunk)
        params = {
            "dimension": [
                f"dx:{dx_str}",
                f"pe:{pe_str}",
                f"ou:{ou_str}",
                "co",
            ],
            "skipMeta": "false",
            "showHierarchy": "true",
            "hierarchyMeta": "true",
            "displayProperty": "NAME",
            "paging": "false",
        }
        label = f"chunk {i}/{len(chunks_to_run)} ({chunk[0]}–{chunk[-1]})"
        data = fetch_chunk_with_retry(params, label)

        if data is None:
            continue

        raw = data.get("rows", [])
        meta = data.get("metaData", {}).get("items", {})
        hier = data.get("metaData", {}).get("ouHierarchy", {})

        for row in raw:
            dx_id, co_id, pe, ou_id, value = row[0], row[1], row[2], row[3], row[4]
            csv_writer.writerow({
                "data_element_id": dx_id,
                "data_element_name": meta.get(dx_id, {}).get("name", dx_id),
                "category_option_combo_id": co_id,
                "category_option_combo_name": meta.get(co_id, {}).get("name", co_id),
                "period": pe,
                "period_type": period_type,
                "org_unit_level": ou_level,
                "org_unit_level_name": ou_level_name,
                "org_unit_id": ou_id,
                "org_unit_name": meta.get(ou_id, {}).get("name", ou_id),
                "org_unit_hierarchy": hier.get(ou_id, ""),
                "value": value,
            })
        csv_file.flush()
        total_for_level += len(raw)

        log_queried_periods(log_path, chunk)

        print(f"    {label}: {len(raw)} rows  (level total: {total_for_level})")
        time.sleep(PAUSE_BETWEEN_CHUNKS)

    csv_file.close()
    return total_for_level

# ── Connect ───────────────────────────────────────────────────────────────────

print("\nConnecting...")
info = get("/system/info")
print(f"System: {info.get('systemName')}  v{info.get('version')}\n")

if args.chunk_override:
    print(f"Chunk override: {args.chunk_override} periods per chunk (all levels)\n")

# ── Selected fistula and prolapse data elements ───────────────────────────────
# Run fetch_fistula_metadata.py first to browse all available variables.
# Then copy IDs of interest here from fistula_metadata.csv.
# See fistula_selected_ids_log.txt for a record of what was selected and why.
#
# IDs are organised into named groups. Each group gets its own "queried periods"
# log under queries/ (fistula_level<N>_<OrgUnitLevel>__<group>.queried). That
# means:
#   - adding a brand-new group below only fetches THAT group's data — periods
#     already logged for existing groups are left alone (no re-fetching, and
#     no duplicate rows appended to the CSVs)
#   - running this from a clean checkout (no queries/ logs yet) walks every
#     group from scratch and ends up with exactly the same combined CSVs
# To extend the pull: add a new named group with its IDs and re-run. Don't
# add IDs to an existing group, rename a group, or move IDs between groups
# once that group has been queried — its log would then be stale and the
# script would either skip data it should fetch or duplicate data it already has.

SELECTED_GROUPS = {
    "obstetric_fistula_and_cord_prolapse": [
        "sRyFDCE2b3P",  # MAT_Obstetric Fistula Cases by Age
        "Rc6kyqCLHUv",  # MAT_Obstetric Fistula Cases Treated by Age
        "CS2F9PJQDbU",  # RMNCH - Obstetric fistula care provided
        "aKcR4pbtypU",  # O60-1353 Labour and delivery complicated by prolapse of cord
        "hNC9qzgEDa0",  # P00-1425 Fetus and newborn affected by prolapsed cord
    ],

    "genital_fistula_and_pelvic_prolapse_icd11": [
        # Fistula
        "v9mSUyDEc0C",  # ESV-ICD11 GC04.10 - Vesicovaginal fistula
        "DOy8h0cZaIB",  # ESV-ICD11 GC04.14 - Urethrovaginal fistula
        "sEvIB4Tto6Y",  # ESV-ICD11 GC04.16 - Rectovaginal fistula
        # Prolapse
        "oPUwDiz9jAU",  # ESV-ICD11 DB31.2 - Rectal prolapse
        "Nbqk9NOhpw3",  # ESV-ICD11 DB53 - Anal prolapse
        "w3yjQC21alO",  # ESV-ICD11 GC40.2 - Prolapse of the vaginal apex
        "ykGyWdc6qG2",  # ESV-ICD11 GC40.3 - Uterovaginal prolapse
        "VV6shblSmaH",  # ESV-ICD11 GC40 - Pelvic organ prolapse
        "cmZDOp42uaF",  # K55-873 Prolapse (Anal prolapse)
        "g5chXpY2vYW",  # K55-874 Prolapse (Rectal prolapse)
        "PV3qk3Pq6YT",  # N80-1137 Prolapse (Female genital prolapse)
    ],
}

ALL_SELECTED_IDS = [id_ for ids in SELECTED_GROUPS.values() for id_ in ids]

if not ALL_SELECTED_IDS:
    sys.exit("No IDs selected. Edit SELECTED_GROUPS in fetch_fistula.py first.")

seen_ids = set()
for group_name, ids in SELECTED_GROUPS.items():
    for id_ in ids:
        if id_ in seen_ids:
            sys.exit(f"Data element {id_} appears in more than one group in SELECTED_GROUPS — "
                     f"each ID must belong to exactly one group, otherwise its rows get pulled twice.")
        seen_ids.add(id_)

print(f"Using {len(ALL_SELECTED_IDS)} selected data elements across {len(SELECTED_GROUPS)} groups:")
for group_name, ids in SELECTED_GROUPS.items():
    print(f"\n  [{group_name}]  ({len(ids)} elements)")
    for id_ in ids:
        print(f"    {id_}")
print()

period_type_groups = {"Monthly": SELECTED_GROUPS}
earliest_date = date(2005, 1, 1)

# ── Generate periods ──────────────────────────────────────────────────────────

today = date.today()
start_year = earliest_date.year

def monthly_periods(sy, ey, em):
    out = []
    for y in range(sy, ey + 1):
        for m in range(1, 13):
            if y == ey and m > em:
                break
            out.append(f"{y}{m:02d}")
    return out

def get_periods(period_type):
    pt = period_type.lower()
    ey = today.year
    em = today.month
    if "yearly" in pt:
        return [str(y) for y in range(start_year, ey + 1)]
    elif "quarterly" in pt or "quarter" in pt:
        out = []
        for y in range(start_year, ey + 1):
            for q in range(1, 5):
                if y == ey and (q - 1) * 3 + 1 > em:
                    break
                out.append(f"{y}Q{q}")
        return out
    else:
        return monthly_periods(start_year, ey, em)

# ── Org unit levels ───────────────────────────────────────────────────────────

print("Fetching organisation unit levels...")
ou_levels_resp = get("/organisationUnitLevels", {
    "fields": "id,name,level",
    "order": "level:asc",
    "paging": "false",
})
ou_levels = [
    l for l in sorted(ou_levels_resp["organisationUnitLevels"], key=lambda x: x["level"])
    if l["level"] in LEVELS_TO_RUN
]
for l in ou_levels:
    effective_chunk = args.chunk_override if args.chunk_override else CHUNK_BY_LEVEL.get(l["level"], DEFAULT_CHUNK)
    print(f"  Level {l['level']}: {l['name']}  (chunk size: {effective_chunk})")
print()

# ── Pull data ─────────────────────────────────────────────────────────────────

grand_total = 0
Path("queries").mkdir(exist_ok=True)

for period_type, groups in period_type_groups.items():
    all_periods = get_periods(period_type)
    total_elements = sum(len(ids) for ids in groups.values())
    print(f"\n══ Period type: {period_type}  "
          f"({total_elements} data elements across {len(groups)} groups, {len(all_periods)} periods) ══")

    for level_info in ou_levels:
        level = level_info["level"]
        level_name = level_info["name"]
        safe_name = level_name.replace(" ", "_")
        csv_path = Path(f"./fistula_level{level}_{safe_name}.csv")

        print(f"\n── Level {level} ({level_name}) → {csv_path.name} ──")

        for group_name, dx_ids in groups.items():
            print(f"\n  ▸ Group '{group_name}' ({len(dx_ids)} elements)")
            n = fetch_level(dx_ids, group_name, period_type, level, level_name, all_periods, csv_path)
            grand_total += n
            print(f"    group done: {n} new rows written")

print(f"\n\nAll done. Grand total rows written this run: {grand_total}")
