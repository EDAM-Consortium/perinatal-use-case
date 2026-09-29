"""
Pulls maternal health, pregnancy outcomes and stillbirth data from the Ethiopian MoH DHIS2 instance.
Collects data at all org unit levels and all available periods.
Saves a separate CSV per level, with adaptive chunk sizes.
Re-run safe: skips periods already queried (whether or not they returned data).

Step 1: Run fetch_maternal_metadata.py to browse all available variables.
Step 2: Copy IDs of interest into SELECTED_IDS below.
Step 3: Run this script.

Usage:
    python fetch_maternal.py --username X --password Y
    python fetch_maternal.py --username X --password Y --chunk-override 3
    python fetch_maternal.py --username X --password Y --levels 2 3
    python fetch_maternal.py --username X --password Y --levels 2 3 --reset-queried
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
LOGIN_URL = "https://dhis.moh.gov.et/dhis-web-commons-security/login.action"

parser = argparse.ArgumentParser()
parser.add_argument("--username", "-u", required=True)
parser.add_argument("--password", "-p", required=True)
parser.add_argument("--chunk-override", type=int, default=None,
                    help="Override chunk size for all levels (e.g. 3, 6, 12). "
                         "Increase on re-runs to fill gaps more efficiently.")
parser.add_argument("--levels", type=int, nargs="+", default=None,
                    help="Only fetch these org unit levels (e.g. --levels 2 3). "
                         "Default: all levels.")
parser.add_argument("--reset-queried", action="store_true",
                    help="Delete the .queried log for each selected level before "
                         "fetching, forcing a full re-fetch. Use when SELECTED_IDS "
                         "has changed and you need fresh data.")
args = parser.parse_args()

class SSLIgnoreAdapter(HTTPAdapter):
    def send(self, *args, **kwargs):
        kwargs["verify"] = False
        return super().send(*args, **kwargs)

session = requests.Session()
session.headers.update({"Accept": "application/json"})
session.mount("http://", SSLIgnoreAdapter())
session.mount("https://", SSLIgnoreAdapter())

def login():
    """Log in via the web form and store the session cookie."""
    resp = session.post(
        LOGIN_URL,
        data={"j_username": args.username, "j_password": args.password},
        allow_redirects=True,
        timeout=120,
    )
    if "JSESSIONID" not in session.cookies:
        sys.exit(f"Login failed — no session cookie returned (HTTP {resp.status_code}). "
                 "Check credentials.")
    print(f"Logged in as {args.username}  (session: {session.cookies['JSESSIONID'][:8]}...)\n")

# Default chunk sizes by org unit level
# Use --chunk-override to increase on re-runs
CHUNK_BY_LEVEL = {
    1: 12,
    2: 12,
    3: 6,
    4: 3,
    5: 1,
    6: 1,
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
    """GET with retry on transient SSL/connection errors."""
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
    """Read the log of all periods already queried (data or no data)."""
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
    """Append a list of periods to the queried log."""
    with open(log_path, "a", encoding="utf-8") as f:
        for p in periods:
            f.write(p + "\n")

def fetch_level(dx_ids, period_type, ou_level, ou_level_name, all_periods, csv_path):
    # Use chunk override if provided, otherwise use level default
    chunk_size = args.chunk_override if args.chunk_override else CHUNK_BY_LEVEL.get(ou_level, DEFAULT_CHUNK)
    log_path = Path("queries") / csv_path.with_suffix(".queried").name

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

        # Log period as queried (success, even if 0 rows)
        log_queried_periods(log_path, chunk)

        print(f"    {label}: {len(raw)} rows  (level total: {total_for_level})")
        time.sleep(PAUSE_BETWEEN_CHUNKS)

    csv_file.close()
    return total_for_level

# ── Connect ───────────────────────────────────────────────────────────────────

print("\nConnecting...")
login()
info = get("/system/info")
print(f"System: {info.get('systemName')}  v{info.get('version')}\n")

if args.chunk_override:
    print(f"Chunk override: {args.chunk_override} periods per chunk (all levels)\n")

# ── 1. Selected maternal health data elements ────────────────────────────────
# Run fetch_maternal_metadata.py first to browse all available variables.
# Then copy IDs of interest here from maternal_metadata.csv.
# See maternal_selected_ids_log.txt for a record of what was selected and why.

SELECTED_IDS = [
    # ── Stillbirth / perinatal death outcomes ────────────────────────────────
    "vqNLnporiZ1",  # MAT_Still Births
    "cFlulEeuKRw",  # O30-1291 Intrauterine Fetal Death (IUFD)
    "lz1lT9MFQd4",  # Z30-1999 Newborn (Single stillbirth)

    # ── Births denominator ───────────────────────────────────────────────────
    "weJvzEsHWWX",  # Z30-1998 Newborn (Single live birth)

    # ── Maternal age — broken down by age category ───────────────────────────
    "lotDArt3TsU",  # MAT_ANC 1st by Maternal Age  (age disaggregation)
    "dCyyyAiuofb",  # MAT_ANC >= 4 by Maternal Age  (age disaggregation)
    "oNLXqTifGKY",  # MAT_Teenagers Tested Positive for Pregnancy
    "mZCqcHS5I0U",  # WBP-T% Teenage Pregnancy Rate (aggregated)

    # ── Parity / gravidity ───────────────────────────────────────────────────
    "JTm0SnNGjms",  # Z30-1994 ANC Supervision of elderly primigravida
    "T5gna2TEX32",  # Z30-1993 ANC Supervision of grand multiparity
    "AxkfDHsp3Fj",  # Z30-1991 ANC Supervision of poor reproductive/obstetric history

    # ── ANC coverage & timing ────────────────────────────────────────────────
    "NTidDdqjEde",  # MAT_ANC >= 8 Contacts
    "hJGEI95aFvT",  # MAT_ANC-1st by Gestational Week  (timing of 1st visit)
    "cmav5iXqALq",  # ANC first visit by gestational week
    "FMHlxx3lBcZ",  # MAT_ANC >= 4 by Gestational Week
    "xE0gwDlZgl5",  # WBP-T% ANC 1st before 12 Weeks Coverage (early booking)
    "zcCEgTBfMIP",  # WBP-T% ANC 4+ Contacts Coverage
    "hfqUwjltEcp",  # WBP-T% ANC 8+ Contacts Coverage
    "mzX0cipJBfa",  # Z30-1989 ANC Supervision of high-risk pregnancy

    # ── Maternal nutrition & micronutrients ──────────────────────────────────
    "y7drS5G9QGT",  # NUT_Pregnant Women Received IFA ≥90 days  (iron–folate)
    "XfbcACv3sB8",  # NUT_Pregnant Women De-Wormed
    "nYcQoY7Kvda",  # FNS_Pregnant women counselled on nutrition during ANC
    "BX8jhiWcgHY",  # ESV-ICD11 JA64 Malnutrition in pregnancy
    "txgF0BhqQsA",  # O20-1255 Malnutrition in pregnancy
    "Gviy3f6fuSR",  # O94-1412 Anaemia complicating pregnancy/childbirth/puerperium
    "CBJVSyU00mz",  # ESV-ICD11 JA65.3 Low weight gain in pregnancy
    "eLRv5LpyuKm",  # O20-1258 Low weight gain in pregnancy

    # ── Hypertensive disorders of pregnancy ──────────────────────────────────
    "onIgJN7FFUn",  # ESV-ICD11 JA24 Pre-eclampsia  (broadest code, most data)
    "NX3tXatAQBk",  # O10-1225 Pre-eclampsia unspecified
    "T269kPxN0G9",  # O10-1223 Severe pre-eclampsia
    "il18oInXFKx",  # O10-1227 Eclampsia in pregnancy
    "wRC6NGAGjTp",  # ESV-ICD11 JA21 Pre-eclampsia superimposed on chronic hypertension
    "YBlOR5P2wXY",  # O10-1217 Pre-existing hypertension complicating pregnancy
    "WERL7LjT4x5",  # O10-1230 Unspecified maternal hypertension

    # ── Diabetes in pregnancy ────────────────────────────────────────────────
    "auXHGN5ToAC",  # ESV-ICD11 JA63 Diabetes mellitus in pregnancy (all types)
    "MMO5hO1SGEc",  # ESV-ICD11 JA63.2 Gestational diabetes mellitus
    "q1ruBjp1W86",  # O20-1249 Diabetes mellitus in pregnancy

    # ── Infections in pregnancy ──────────────────────────────────────────────
    "kbVb7uFcWuH",  # MAT_Pregnant Women Tested for Syphilis
    "Q831ZIXPYSX",  # MAT_Pregnant Women Treated for Syphilis
    "XfkilS4FUL1",  # O94-1408 Syphilis complicating pregnancy
    "rlHrL4d6AN3",  # O20-1246 Urinary tract infection in pregnancy
    "qgY0HMjUdLG",  # O20-1245 Pyelonephritis in pregnancy
    "JBq6A6H5yiu",  # ESV-ICD11 JA62 Infections of genitourinary tract in pregnancy
    "Uy3sUGYDocU",  # O94-1410 HIV disease complicating pregnancy

    # ── Haemorrhage & placental complications ────────────────────────────────
    "hJaDtuUyuAq",  # ESV-ICD11 JA41 Antepartum haemorrhage  (broad code)
    "zF8m83W0plG",  # O30-1305 Antepartum haemorrhage NEC
    "IEjeuIGU58m",  # O30-1303 Placenta praevia with haemorrhage
    "hM79L2Fffwb",  # O30-1302 Placenta praevia without haemorrhage
    "Wa1XuXurAEh",  # O60-1345 Intrapartum haemorrhage
    "xOMNqgdHMJq",  # MAT_Postpartum Hemorrhage (PPH)

    # ── Premature rupture of membranes ───────────────────────────────────────
    "qx7r284km0B",  # ESV-ICD11 JA89 Maternal care for premature rupture of membranes

    # ── Prolonged / complicated labour ───────────────────────────────────────
    "KZeJvQis9Ng",  # ESV-ICD11 JB03.0 Prolonged first stage of labour
    "pc1rrYIDEZX",  # ESV-ICD11 JB03.1 Prolonged second stage of labour
    "zgTTh8iGsQP",  # O60-1325 Prolonged first stage of labour
    "et6xmt51VFr",  # O60-1326 Prolonged second stage of labour
    "eSvo4q5gSO5",  # ESV-ICD11 JB05 Obstructed labour due to maternal pelvic abnormality
    "uAyi1noyZWZ",  # ESV-ICD11 JB04 Obstructed labour due to malposition/malpresentation
    "FsCxrT3WGTz",  # O60-1343 Obstructed labour unspecified
    "G8BtoAHKUI6",  # ESV-ICD11 JB0A.1 Rupture of uterus during labour
    "o0VKXcMiHAd",  # O60-1356 Uterine rupture during labour
    "dQ9ZONjeTuz",  # O60-1355 Uterine rupture before onset of labour

    # ── Fetal distress & asphyxia ────────────────────────────────────────────
    "PG2RQZtyZv9",  # ESV-ICD11 JB07 Labour complicated by fetal distress
    "VT49tM7NMm0",  # O60-1346 Labour complicated by fetal stress/distress
    "Z37i0wwAvjH",  # O60-1348 Labour complicated by meconium in amniotic fluid
    "KSrBwgS4DQC",  # P20-1466 Intrauterine hypoxia noted before onset of labour

    # ── Cord complications ───────────────────────────────────────────────────
    "aKcR4pbtypU",  # O60-1353 Labour and delivery complicated by cord prolapse
    "LGNOxm3PPDs",  # ESV-ICD11 JB08 Labour complicated by umbilical cord complications

    # ── Preterm birth & low birth weight ─────────────────────────────────────
    "XpqxTfhsVWL",  # ESV-ICD11 JB00 Preterm labour or delivery
    "FDTfcU6kqcY",  # O60-1311 Preterm labour and delivery
    "vWJ2dJT7AtI",  # ESV-ICD11 KA21.4 Preterm newborn
    "HnLMGD0DnU0",  # ESV-ICD11 KA21.2 Low birth weight of newborn
    "j6GEdB3OXWZ",  # CH_Newborns < 2000 g and/or premature with KMC initiated
    "m0j9dD48jVP",  # O30-1310 Prolonged (post-term) pregnancy

    # ── Fetal growth restriction ─────────────────────────────────────────────
    "Qn9jp4jiZPt",  # O30-1292 Maternal care for poor fetal growth
    "RWulXW939kX",  # O30-1290 Maternal care for signs of fetal hypoxia
    "SH4gkjJ9Xnc",  # ESV-ICD11 KA20 Disorders of newborn related to slow fetal growth/malnutrition

    # ── Multiple gestation ───────────────────────────────────────────────────
    "G2h7hAurvnY",  # ESV-ICD11 JA80.0 Twin pregnancy
    "G1Jh6qEesmr",  # O30-1273 Twin pregnancy
    "Ccg95wwkP2I",  # ESV-ICD11 JA87 Polyhydramnios
    "pMaIDtVcadf",  # ESV-ICD11 JA88 Disorders of amniotic fluid/membranes

    # ── Malpresentation & dystocia ───────────────────────────────────────────
    "yZweNVKF450",  # O30-1278 Maternal care for breech presentation
    "lH6LbkboZsO",  # O30-1279 Maternal care for transverse and oblique lie
    "yRYcATaIBBZ",  # O30-1287 Maternal care for cervical incompetence

    # ── Rhesus isoimmunisation ───────────────────────────────────────────────
    "uYTTNfHAZ18",  # O30-1289 Maternal care for rhesus isoimmunisation

    # ── Delivery care ────────────────────────────────────────────────────────
    "lzhOxCHJMOV",  # MAT_Skilled Birth Attendance
    "VAwGaDJTcbm",  # MAT_Births By Caesarean Section

    # ── Obstetric fistula (marker of severe obstructed labour) ───────────────
    "sRyFDCE2b3P",  # MAT_Obstetric Fistula Cases by Age
    "Rc6kyqCLHUv",  # MAT_Obstetric Fistula Cases Treated by Age

    # ── Lifestyle / substance use ────────────────────────────────────────────
    "Qf7iQv49C4C",  # WBP-CF Substance use disorder — Alcohol
    "vdFj1JNnuJA",  # WBP-CF Substance use disorder — Khat (chat)
    "oZuiC0yCz8F",  # WBP-CF Substance use disorder — Tobacco
    "lv3PWuVsAC5",  # WBP-T% Individuals treated for Substance Use & Alcohol (aggregate)

    # ── Maternal death (severe-outcome context) ──────────────────────────────
    "pj5uAe3P8EM",  # MAT_Maternal Deaths in Health Facility
    "VbLl2M9cuzV",  # MAT_Maternal Deaths in the Community
]

if not SELECTED_IDS:
    sys.exit("No IDs selected. Edit SELECTED_IDS in fetch_maternal.py first.")

print(f"Using {len(SELECTED_IDS)} selected data elements:")
for id_ in SELECTED_IDS:
    print(f"  {id_}")
print()

# All selected IDs treated as a single Monthly group
period_type_groups = {"Monthly": SELECTED_IDS}
earliest_date = date(2005, 1, 1)

# ── 3. Generate periods ───────────────────────────────────────────────────────

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

def quarterly_periods(sy, ey):
    out = []
    for y in range(sy, ey + 1):
        for q in range(1, 5):
            if y == ey and (q - 1) * 3 + 1 > today.month:
                break
            out.append(f"{y}Q{q}")
    return out

def yearly_periods(sy, ey):
    return [str(y) for y in range(sy, ey + 1)]

def sixmonthly_periods(sy, ey):
    out = []
    for y in range(sy, ey + 1):
        for s in [1, 2]:
            if y == ey and s == 2 and today.month < 7:
                break
            out.append(f"{y}S{s}")
    return out

def get_periods(period_type):
    pt = period_type.lower()
    ey = today.year
    em = today.month
    if "yearly" in pt or pt in ("financialjuly", "financialapr", "financialoct"):
        return yearly_periods(start_year, ey)
    elif "sixmonthly" in pt:
        return sixmonthly_periods(start_year, ey)
    elif "quarterly" in pt or "quarter" in pt:
        return quarterly_periods(start_year, ey)
    elif "weekly" in pt:
        return [f"{y}W01;{y}W53" for y in range(start_year, ey + 1)]
    else:
        return monthly_periods(start_year, ey, em)

# ── 4. Org unit levels ────────────────────────────────────────────────────────

print("Fetching organisation unit levels...")
ou_levels_resp = get("/organisationUnitLevels", {
    "fields": "id,name,level",
    "order": "level:asc",
    "paging": "false",
})
ou_levels = sorted(ou_levels_resp["organisationUnitLevels"], key=lambda x: x["level"])

if args.levels:
    ou_levels = [l for l in ou_levels if l["level"] in args.levels]
    if not ou_levels:
        sys.exit(f"No org unit levels matched --levels {args.levels}. Check level numbers.")

for l in ou_levels:
    effective_chunk = args.chunk_override if args.chunk_override else CHUNK_BY_LEVEL.get(l["level"], DEFAULT_CHUNK)
    print(f"  Level {l['level']}: {l['name']}  (chunk size: {effective_chunk})")
print()

# ── 5. Pull data ──────────────────────────────────────────────────────────────

grand_total = 0
Path("queries").mkdir(exist_ok=True)

for period_type, dx_ids in period_type_groups.items():
    all_periods = get_periods(period_type)
    print(f"\n══ Period type: {period_type}  ({len(dx_ids)} data elements, {len(all_periods)} periods) ══")

    for level_info in ou_levels:
        level = level_info["level"]
        level_name = level_info["name"]
        safe_name = level_name.replace(" ", "_")
        csv_path = Path(f"./maternal_level{level}_{safe_name}.csv")
        log_path = Path("queries") / csv_path.with_suffix(".queried").name

        if args.reset_queried:
            if log_path.exists():
                log_path.unlink()
                print(f"  Cleared queried log: {log_path.name}")
            if csv_path.exists():
                backup = csv_path.with_suffix(".csv.bak")
                csv_path.rename(backup)
                print(f"  Backed up existing CSV → {backup.name}")

        print(f"\n── Level {level} ({level_name}) → {csv_path.name} ──")
        n = fetch_level(dx_ids, period_type, level, level_name, all_periods, csv_path)
        grand_total += n
        print(f"  Level {level} done: {n} new rows written")

print(f"\n\nAll done. Grand total rows written this run: {grand_total}")
