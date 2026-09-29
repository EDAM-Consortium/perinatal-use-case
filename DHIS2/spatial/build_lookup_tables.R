### BUILD ADMIN LOOKUP TABLE — DHIS2
# Maps DHIS2 region_name values -> OCHA admin1 p-codes.
#
# DHIS2's only reliably clean location field is region_name. Its zone-level
# field (org_unit_name, "level 3" of the DHIS2 hierarchy) is a heterogeneous
# mix of true zones, woredas, sub-cities, towns and individually named
# hospitals/health facilities — DHIS2's hierarchy depth varies by region (city
# administrations have flatter hierarchies, so sub-cities/hospitals end up at
# "level 3" alongside true zones elsewhere). Matching that field to OCHA
# admin2/admin3 polygons would mean guessing locations for the roughly one
# third of units that are individual facilities rather than areas — and
# checking the fetched DHIS2 org-unit geometries (../data/spatial/) shows only
# about half of the 199 reporting units appear there either. Since the user
# only needs *some* subnational resolution to map stillbirths in space, this
# lookup stops at admin1 (region) — the one level DHIS2 reports unambiguously
# and completely, with no guesswork.
#
# All 14 DHIS2 region_name values match a CURRENT (post-2023) OCHA admin1
# polygon one-to-one. Unlike EDHS's older survey waves (which used the
# pre-2023 SNNPR and so can't be mapped to a single current polygon), DHIS2
# already reports using the post-2023 administrative structure throughout, so
# there is no historic region-split ambiguity to resolve here.
#
# Run with the working directory set to this folder.
# Output: lookup_admin1.csv

library(dplyr)

admin1_lookup <- tribble(
  ~dataset_region,                    ~adm1_name,            ~adm1_pcode, ~match_notes,
  "Addis Ababa City Administration",  "Addis Ababa",          "ET14",     "suffix stripped",
  "Afar Region",                      "Afar",                 "ET02",     "suffix stripped",
  "Amhara Region",                    "Amhara",               "ET03",     "suffix stripped",
  "Benishangul Gumuz Region",         "Benishangul-Gumuz",    "ET06",     "suffix stripped; spelling variant (hyphen)",
  "Central Ethiopian Region",         "Central Ethiopia",     "ET07",     "suffix stripped; spelling variant (Ethiopian/Ethiopia)",
  "Dire Dawa City Administration",    "Dire Dawa",            "ET15",     "suffix stripped",
  "Gambella Region",                  "Gambela",              "ET12",     "suffix stripped; spelling variant",
  "Harari Region",                    "Harari",               "ET13",     "suffix stripped",
  "Oromia Region",                    "Oromia",               "ET04",     "suffix stripped",
  "Sidama Region",                    "Sidama",               "ET16",     "suffix stripped",
  "Somali Region",                    "Somali",               "ET05",     "suffix stripped",
  "South Ethiopia Region",            "South Ethiopia",       "ET08",     "suffix stripped",
  "South West Ethiopia Region",       "South West Ethiopia",  "ET11",     "suffix stripped",
  "Tigray Region",                    "Tigray",               "ET01",     "suffix stripped"
)

write.csv(admin1_lookup, "lookup_admin1.csv", row.names = FALSE)
cat("Saved lookup_admin1.csv —", nrow(admin1_lookup),
    "regions, all matched 1:1 to a current OCHA admin1 polygon\n")
