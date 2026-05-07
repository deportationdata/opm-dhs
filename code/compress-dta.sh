#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

STATA="${STATA:-/Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp}"

DO_FILE="compress-dta-$$.do"
LOG_FILE="${DO_FILE%.do}.log"
trap 'rm -f "$DO_FILE" "$LOG_FILE"' EXIT

cat > "$DO_FILE" <<'EOF'
set more off
foreach f in ///
    "data/opm-accessions-immigration.dta" ///
    "data/opm-employment-immigration.dta" ///
    "data/opm-separations-immigration.dta" {
    capture confirm file "`f'"
    if _rc continue
    use "`f'", clear
    quietly ds, has(type string)
    foreach v in `r(varlist)' {
        capture encode `v', generate(__tmp_encode)
        if _rc {
            display "Skipped encode for `v' in `f' (rc=" _rc ")"
        }
        else {
            drop `v'
            rename __tmp_encode `v'
        }
    }
    compress
    save "`f'", replace
}
EOF

"$STATA" -b do "$DO_FILE"
