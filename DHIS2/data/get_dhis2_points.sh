#!/bin/bash
# Downloads DHIS2 organisation unit point geometries for all levels (1-6)
# from the Ethiopian MoH DHIS2 instance.
# Output: spatial/ethiopia_dhis2_level1_points.geojson through spatial/ethiopia_dhis2_level6_points.geojson

USERNAME="Birhana"
PASSWORD="Dhis2_12345"
BASE_URL="https://dhis.moh.gov.et"

mkdir -p spatial

echo "Authenticating..."
curl -k -c cookies_temp.txt -u "${USERNAME}:${PASSWORD}" \
  "${BASE_URL}/api/system/info" -o /dev/null -s

for LEVEL in 1 2 3 4 5 6; do
  OUTPUT="spatial/ethiopia_dhis2_level${LEVEL}_points.geojson"
  echo "Downloading level ${LEVEL}..."
  curl -k -b cookies_temp.txt \
    "${BASE_URL}/api/organisationUnits.geojson?level=${LEVEL}&includeDescendants=false" \
    -o "${OUTPUT}"
  echo "  Saved to ${OUTPUT}"
done

rm cookies_temp.txt
echo "Done. All files saved to spatial/"
