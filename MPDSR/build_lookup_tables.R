### BUILD ADMIN LOOKUP TABLES
# Maps dataset location names -> OCHA p-codes for admin 1, 2, 3
# Output: lookup_admin1.csv, lookup_admin2.csv, lookup_admin3.csv

library(readxl)
library(sf)
library(dplyr)

df   <- read_excel("Final data set_mpdsr.xlsx", sheet = "Final data set_1")
adm1 <- st_drop_geometry(read_sf("OCHA/eth_admin1.shp"))
adm2 <- st_drop_geometry(read_sf("OCHA/eth_admin2.shp"))
adm3 <- st_drop_geometry(read_sf("OCHA/eth_admin3.shp"))

# ── ADMIN 1 ──────────────────────────────────────────────────────────────────
# Hand-coded: 12 unique values in dataset

admin1_lookup <- tribble(
  ~dataset_region,      ~adm1_name,           ~adm1_pcode, ~match_notes,
  "Addis Ababa",        "Addis Ababa",         "ET14",      "",
  "Afar",               "Afar",                "ET02",      "",
  "Amhara",             "Amhara",              "ET03",      "",
  "Benishangul Gumuz",  "Benishangul-Gumuz",   "ET06",      "spelling variant",
  "Dire Dawa",          "Dire Dawa",           "ET15",      "",
  "Gambella",           "Gambela",             "ET12",      "spelling variant",
  "Hareri",             "Harari",              "ET13",      "spelling variant",
  "Oromia",             "Oromia",              "ET04",      "",
  "SNNP",               NA,                    NA,          "pre-2023 SNNP region — now split into Central Ethiopia (ET07), South Ethiopia (ET08), South West Ethiopia (ET11), Sidama (ET16); recode using zone/woreda",
  "Somali",             "Somali",              "ET05",      "",
  "Somalia",            "Somali",              "ET05",      "likely data-entry variant of Somali",
  "Tigray",             "Tigray",              "ET01",      ""
)

write.csv(admin1_lookup, "lookup_admin1.csv", row.names = FALSE)
cat("Saved lookup_admin1.csv\n")

# ── ADMIN 2 ──────────────────────────────────────────────────────────────────
# Addis Ababa sub-cities all map to Region 14 (ET1401) at admin2 level

addis_subcities <- c("Addis Ketema", "Akaki Kaliti", "Arada", "Bole",
                     "Chirkos", "Gulele", "Kolfe Keraniyo", "Lideta",
                     "LIDETA", "Nefas Silk Lafto", "Yeka")

admin2_lookup <- tribble(
  ~dataset_zone,         ~adm2_name,                       ~adm2_pcode, ~adm1_name,           ~adm1_pcode, ~match_notes,

  # Tigray
  "North Western Tigray", "North Western",                 "ET0101",    "Tigray",              "ET01",     "name trimmed in shapefile",
  "Central Tigray",       "Central",                       "ET0102",    "Tigray",              "ET01",     "name trimmed in shapefile",
  "Eastern Tigray",       "Eastern",                       "ET0103",    "Tigray",              "ET01",     "name trimmed in shapefile",
  "South Tigray",         "Southern",                      "ET0104",    "Tigray",              "ET01",     "name trimmed in shapefile",
  "South East Tigray",    "South Eastern",                 "ET0106",    "Tigray",              "ET01",     "name trimmed in shapefile",
  "Mekele Especial Zone", "Mekelle",                       "ET0107",    "Tigray",              "ET01",     "spelling/format variant",
  "Western Tigray",       NA,                              NA,          "Tigray",              "ET01",     "Western Tigray not in OCHA shapefile; area may be contested/absorbed into North Western (ET0101)",

  # Afar
  "Zone 01",              "Awsi /Zone 1",                  "ET0201",    "Afar",                "ET02",     "",
  "Zone 02",              "Kilbati /Zone 2",               "ET0202",    "Afar",                "ET02",     "",
  "Zone 03",              "Gabi /Zone 3",                  "ET0203",    "Afar",                "ET02",     "",
  "Zone 04",              "Fanti /Zone 4",                 "ET0204",    "Afar",                "ET02",     "",
  "Zone 05",              "Hari /Zone 5",                  "ET0205",    "Afar",                "ET02",     "",
  "dollol",               "Doolo",                         "ET0507",    "Somali",              "ET05",     "spelling variant — Doolo is in Somali, not Afar; verify region in source data",

  # Amhara
  "North Gondar",         "North Gondar",                  "ET0301",    "Amhara",              "ET03",     "",
  "South Gonder",         "South Gondar",                  "ET0302",    "Amhara",              "ET03",     "spelling variant",
  "North Wollo",          "North Wello",                   "ET0303",    "Amhara",              "ET03",     "spelling variant",
  "South Wolo",           "South Wello",                   "ET0304",    "Amhara",              "ET03",     "spelling variant",
  "North Shewa",          NA,                              NA,          NA,                    NA,         "AMBIGUOUS — could be North Shewa (AM) ET0305 in Amhara or North Shewa (OR) ET0406 in Oromia; check Region column",
  "East Gojjam",          "East Gojam",                    "ET0306",    "Amhara",              "ET03",     "spelling variant",
  "West Gojjam",          "West Gojam",                    "ET0307",    "Amhara",              "ET03",     "spelling variant",
  "Wag Himra",            "Wag Hamra",                     "ET0308",    "Amhara",              "ET03",     "spelling variant",
  "Awi",                  "Awi",                           "ET0309",    "Amhara",              "ET03",     "",
  "Oromia",               "Oromo Nationality Administration","ET0310",  "Amhara",              "ET03",     "Oromia nationality zone within Amhara",
  "Oromia spe",           "Oromo Nationality Administration","ET0310",  "Amhara",              "ET03",     "likely Oromia special zone in Amhara",
  "Central Gondar",       "Central Gondar",                "ET0311",    "Amhara",              "ET03",     "",
  "Gondar town",          "Central Gondar",                "ET0311",    "Amhara",              "ET03",     "Gondar city sits within Central Gondar; no separate shapefile polygon",
  "Bahir Dar",            "Bahir Dar town Admin",          "ET0314",    "Amhara",              "ET03",     "town admin zone",
  "Dessie",               "North Wello",                   "ET0303",    "Amhara",              "ET03",     "Dessie is capital of North Wello; no separate zone polygon — verify",

  # Oromia
  "West Wellega",         "West Wellega",                  "ET0401",    "Oromia",              "ET04",     "",
  "East Wellega",         "East Wellega",                  "ET0402",    "Oromia",              "ET04",     "",
  "Ilu Aba Bora",         "Ilu Aba Bora",                  "ET0403",    "Oromia",              "ET04",     "",
  "Jimma",                "Jimma",                         "ET0404",    "Oromia",              "ET04",     "",
  "Jimma Sp.",            "Jimma",                         "ET0404",    "Oromia",              "ET04",     "Jimma special zone — mapped to Jimma zone",
  "West Shewa",           "West Shewa",                    "ET0405",    "Oromia",              "ET04",     "",
  "East Shewa",           "East Shewa",                    "ET0407",    "Oromia",              "ET04",     "",
  "Adama",                "East Shewa",                    "ET0407",    "Oromia",              "ET04",     "Adama (Nazret) is capital of East Shewa; no separate zone polygon",
  "Arsi",                 "Arsi",                          "ET0408",    "Oromia",              "ET04",     "",
  "West Hararge",         "West Hararge",                  "ET0409",    "Oromia",              "ET04",     "",
  "East Hararge",         "East Hararge",                  "ET0410",    "Oromia",              "ET04",     "",
  "Bale",                 "Bale",                          "ET0411",    "Oromia",              "ET04",     "",
  "Borena",               "Borena",                        "ET0412",    "Oromia",              "ET04",     "",
  "South West Shewa",     "South West Shewa",              "ET0413",    "Oromia",              "ET04",     "",
  "Guji",                 "Guji",                          "ET0414",    "Oromia",              "ET04",     "",
  "West Arsi",            "West Arsi",                     "ET0417",    "Oromia",              "ET04",     "",
  "Qeleme Wellega",       "Kelem Wellega",                 "ET0418",    "Oromia",              "ET04",     "spelling variant",
  "Horo Gudru Wellega",   "Horo Gudru Wellega",            "ET0419",    "Oromia",              "ET04",     "",

  # Somali
  "Fafan",                "Fafan",                         "ET0502",    "Somali",              "ET05",     "",
  "Jijiga",               "Fafan",                         "ET0502",    "Somali",              "ET05",     "Jijiga zone renamed to Fafan",
  "Liban",                "Liban",                         "ET0509",    "Somali",              "ET05",     "",
  "Liben",                "Liban",                         "ET0509",    "Somali",              "ET05",     "spelling variant",

  # Benishangul-Gumuz
  "Metekel",              "Metekel",                       "ET0602",    "Benishangul-Gumuz",   "ET06",     "",
  "Assosa",               "Assosa",                        "ET0603",    "Benishangul-Gumuz",   "ET06",     "",
  "Kamashi",              "Kamashi",                       "ET0604",    "Benishangul-Gumuz",   "ET06",     "",
  "Maokomo",              "Mao-komo Special",              "ET0605",    "Benishangul-Gumuz",   "ET06",     "spelling variant",

  # Central Ethiopia (formerly SNNP)
  "Gurage",               "Guraghe",                       "ET0701",    "Central Ethiopia",    "ET07",     "spelling variant",
  "Hadiya",               "Hadiya",                        "ET0702",    "Central Ethiopia",    "ET07",     "",
  "Kembata Timbaro",      "Kembata",                       "ET0703",    "Central Ethiopia",    "ET07",     "zone renamed/trimmed",
  "Alaba",                "Halaba",                        "ET0714",    "Central Ethiopia",    "ET07",     "spelling variant",
  "Siliti",               "Siltie",                        "ET0720",    "Central Ethiopia",    "ET07",     "spelling variant",

  # South Ethiopia (formerly SNNP)
  "Wolayita",             "Wolayita",                      "ET0801",    "South Ethiopia",      "ET08",     "",
  "Gamo Gofa",            NA,                              NA,          "South Ethiopia",      "ET08",     "pre-split zone; now Gamo (ET0802) + Gofa (ET0803); recode using woreda",
  "Basketo",              "Basketo",                       "ET0804",    "South Ethiopia",      "ET08",     "",
  "Derashe",              "Derashe",                       "ET0807",    "South Ethiopia",      "ET08",     "",
  "Konso",                "Konso",                         "ET0809",    "South Ethiopia",      "ET08",     "",
  "Gedeo",                "Gedeo",                         "ET0811",    "South Ethiopia",      "ET08",     "",
  "South Omo",            "South Omo",                     "ET0812",    "South Ethiopia",      "ET08",     "",
  "Segen",                NA,                              NA,          "South Ethiopia",      "ET08",     "old Segen Area People's Zone — now split; includes Konso (ET0809), Derashe (ET0807), Burji (ET0810), Alle (ET0806)",
  "Segen People",         NA,                              NA,          "South Ethiopia",      "ET08",     "same as Segen above",

  # South West Ethiopia (formerly SNNP)
  "SHEKA",                "Sheka",                         "ET1101",    "South West Ethiopia", "ET11",     "spelling variant",
  "Kefa",                 "Kefa",                          "ET1102",    "South West Ethiopia", "ET11",     "",
  "Bench Maji",           "Bench Sheko",                   "ET1103",    "South West Ethiopia", "ET11",     "zone renamed from Bench Maji to Bench Sheko",
  "Dawuro",               "Dawuro",                        "ET1104",    "South West Ethiopia", "ET11",     "",

  # Gambella
  "Nuer",                 "Nuwer",                         "ET1201",    "Gambela",             "ET12",     "spelling variant",
  "Aguawak",              "Agnewak",                       "ET1202",    "Gambela",             "ET12",     "spelling variant (Anuak/Agnewak zone)",
  "Megenger",             "Majang",                        "ET1203",    "Gambela",             "ET12",     "spelling variant (Mejenger/Majang zone)",
  "Mejenger",             "Majang",                        "ET1203",    "Gambela",             "ET12",     "spelling variant",

  # Harari
  "Hareri",               "Harari",                        "ET1301",    "Harari",              "ET13",     "spelling variant",
  "Harari",               "Harari",                        "ET1301",    "Harari",              "ET13",     "",

  # Dire Dawa
  "Dire Dawa",            "Dire Dawa urban",               "ET1501",    "Dire Dawa",           "ET15",     "zone-level data mapped to urban polygon; rural is ET1502",

  # Sidama
  "Awassa",               "Hawassa town Admin",            "ET1601",    "Sidama",              "ET16",     "Awassa = Hawassa; town admin zone",
  "Sidama",               NA,                              NA,          "Sidama",              "ET16",     "Sidama is now its own region (ET16); no single admin2 polygon covers whole region"
)

# Add Addis Ababa sub-cities
addis_rows <- tibble(
  dataset_zone = addis_subcities,
  adm2_name    = "Region 14",
  adm2_pcode   = "ET1401",
  adm1_name    = "Addis Ababa",
  adm1_pcode   = "ET14",
  match_notes  = "Addis Ababa sub-city; mapped to the single admin2 polygon for Addis Ababa"
)

admin2_lookup <- bind_rows(admin2_lookup, addis_rows) %>%
  arrange(adm1_pcode, dataset_zone)

write.csv(admin2_lookup, "lookup_admin2.csv", row.names = FALSE)
cat("Saved lookup_admin2.csv\n")

# ── ADMIN 3 ──────────────────────────────────────────────────────────────────
# Fuzzy-match 581 dataset woredas against 1148 shapefile woredas

if (!requireNamespace("stringdist", quietly = TRUE)) install.packages("stringdist")
library(stringdist)

data_woredas <- sort(unique(df$woreda))
shp_woredas  <- adm3$adm3_name

# For each data woreda, find the closest shapefile match by Jaro-Winkler distance
match_idx  <- amatch(tolower(data_woredas), tolower(shp_woredas), maxDist = 0.3,
                     method = "jw")
match_dist <- stringdist(tolower(data_woredas),
                         tolower(shp_woredas[match_idx]), method = "jw")

admin3_lookup <- tibble(
  dataset_woreda = data_woredas,
  adm3_name      = adm3$adm3_name[match_idx],
  adm3_pcode     = adm3$adm3_pcode[match_idx],
  adm2_name      = adm3$adm2_name[match_idx],
  adm2_pcode     = adm3$adm2_pcode[match_idx],
  adm1_name      = adm3$adm1_name[match_idx],
  adm1_pcode     = adm3$adm1_pcode[match_idx],
  match_distance = round(match_dist, 3),
  match_quality  = case_when(
    is.na(match_idx)   ~ "no_match",
    match_dist == 0    ~ "exact",
    match_dist < 0.05  ~ "high",
    match_dist < 0.15  ~ "medium",
    TRUE               ~ "low_review_needed"
  )
)

write.csv(admin3_lookup, "lookup_admin3.csv", row.names = FALSE)
cat("Saved lookup_admin3.csv\n")

# Summary
cat("\n=== Admin3 match quality summary ===\n")
print(table(admin3_lookup$match_quality))
cat("\nRows needing review (low quality or no match):\n")
admin3_lookup %>%
  filter(match_quality %in% c("low_review_needed", "no_match")) %>%
  select(dataset_woreda, adm3_name, match_distance, match_quality) %>%
  print(n = 100)
