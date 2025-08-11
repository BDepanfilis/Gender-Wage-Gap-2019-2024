# run_all.R
# Runs the whole project in order

source("r_scripts/Wrangle.R")   # builds outputs/<year>/cpsmar_e.csv
source("r_scripts/Analysis.R")  # writes outputs/tables/*.csv + dashboard/*.png

cat("\n✓ Done. Charts in /dashboard, tables in /outputs/tables\n")
