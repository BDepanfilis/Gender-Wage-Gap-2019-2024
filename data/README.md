# Data README — CPS ASEC (2019–2024)

This project uses the **Current Population Survey (CPS) March ASEC** public-use microdata to estimate the gender wage gap for full-time workers in the U.S.

## What to download

For each year **2019–2024**, download and unzip the CPS ASEC **public-use CSV package**. From each year’s zip, this project uses exactly two files:

- `pppubYY.csv`  — **Person** file  
- `hhpubYY.csv`  — **Household** file

Where `YY` is the two-digit year (e.g., `19`, `20`, …, `24`).

You can obtain these from the U.S. Census Bureau’s CPS ASEC public-use data page (search: *CPS ASEC public use CSV 2019* etc.). Download the zips for each year, extract, and copy the two CSVs into this `data/` folder.

## Expected filenames (exact)

```
data/
  pppub19.csv   hhpub19.csv
  pppub20.csv   hhpub20.csv
  pppub21.csv   hhpub21.csv
  pppub22.csv   hhpub22.csv
  pppub23.csv   hhpub23.csv
  pppub24.csv   hhpub24.csv
```

> Filenames must match exactly (lowercase) so `r_scripts/Wrangle.R` can find them.

## Variables the code uses

From the **person** file (`pppubYY.csv`):

- `A_AGE` (age), `PEARNVAL` (annual earnings), `HRSWK` (hours/week), `WKSWORK` (weeks)
- `A_SEX` (sex), `PEHSPNON` (Hispanic), `PRDTRACE` (race)
- `A_MARITL` (marital status), `A_HGA` (education)
- `A_UNMEM` (union member), `A_UNCOV` (union coverage)
- `A_MJOCC` (major occupation), `PH_SEQ` (household seq / join key)

From the **household** file (`hhpubYY.csv`):

- `GEREG` (region), `H_SEQ` (household seq), `GTCO` (county), `GTCBSAST` (metro status)

The wrangling script:
- joins household → person on `H_SEQ`/`PH_SEQ`,  
- derives flags (e.g., `female`, `fulltime`, education dummies),  
- filters to **full-time** workers (≥48 weeks & ≥36 hours), then  
- writes `outputs/<year>/cpsmar_e.csv`.

## How to reproduce

1) Place all required CSVs (above) in `data/`.  
2) From the repo root, run:

```bash
Rscript r_scripts/Wrangle.R
Rscript r_scripts/analysis.R
# or
Rscript r_scripts/run.R    # if you added the small runner
```

This will generate:

- Per-year extracts: `outputs/<year>/cpsmar_e.csv` (not committed)  
- Tables: `outputs/tables/gaps_by_spec.csv`, `outputs/tables/gap_trend.csv`  
- Figures: `dashboard/*.png` (used in `docs/index.html`)

After running `Wrangle.R`, you should see folders `outputs/2019` … `outputs/2024` each containing `cpsmar_e.csv`.

## Notes

- Data are **public-use** CPS microdata from the U.S. Census Bureau.  
- All results in this repo are **reproducible** from these inputs; no proprietary data are used.  
- To extend to another year, add `pppubYY.csv` and `hhpubYY.csv` for that year and update the `years` vector in the R scripts.
