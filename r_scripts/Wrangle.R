# ------------------------------------------------------------------------
# Source file:  Wrangle.R
# Data files:   pppub19.csv, hhpub19.csv (from asecpub19csv.zip) - 2024
# Output files: LF.csv, cpsmar_e.out, cpsmar_e.csv
# Content:      Creates March CPS (ASEC Supplement) extract
# Notes:        Applied to ASEC March 2019-2024 surveys
# Authors:      Bradley DePanfilis
# Last updated: 10 Aug 2025
# -------------------------------------------------------------------------

# Load required packages 

library(tidyverse)  # For tidyverse/dplyr verbs
library(here)       # For easy file referencing in project workflows

#-------------------------------------------------------------------------------

process_cps_asec <- function(year) {
  message(paste0("Processing CPS March ASEC ", year))
  
  # Build file paths
  ppfile <- here("data", paste0("pppub", substr(year, 3, 4), ".csv"))
  hhfile <- here("data", paste0("hhpub", substr(year, 3, 4), ".csv"))
  
  # Read person and household files
  cpsmar <- read_csv(ppfile)
  hhcps <- read_csv(hhfile) %>% select("GEREG", "H_SEQ", "GTCO", "GTCBSAST")
  
  # Compute LFPR
  E    <- nrow(cpsmar[cpsmar$PEMLR %in% c(1,2) & cpsmar$A_AGE > 15,])
  U    <- nrow(cpsmar[cpsmar$PEMLR %in% c(3,4) & cpsmar$A_AGE > 15,])
  LF   <- E + U
  NILF <- nrow(cpsmar[cpsmar$PEMLR %in% c(5,6,7) & cpsmar$A_AGE > 15,])
  Pop  <- NILF + LF
  LFPR <- (LF / Pop) * 100
  LFdf <- data.frame(E, U, LF, NILF, Pop, LFPR)
  
  outdir <- here("outputs", as.character(year))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  
  write_csv(LFdf, file.path(outdir, "LF.csv"))
  
  # Create extract
  cpsmar_e <- cpsmar %>%
    rename(
      age       = A_AGE,
      earnings  = PEARNVAL,
      hours     = HRSWK,
      weeks     = WKSWORK,
      race      = PRDTRACE,
      marital   = A_MARITL,
      education = A_HGA,
      H_SEQ     = PH_SEQ
    ) %>%
    mutate(
      female   = if_else(A_SEX==2, 1, 0),
      hisp     = if_else(PEHSPNON==1, 1, 0),
      fulltime = if_else(weeks >= 48 & hours >= 36, 1, 0),
      union    = if_else(A_UNMEM==1, 1, 0),
      uncov    = if_else(A_UNCOV==1, 1, 0),
      HSGrad   = if_else(education == 39, 1, 0),
      SomeColl = if_else(education >= 40 & education <=42, 1, 0),
      CollDeg  = if_else(education >= 43, 1, 0)
    ) %>%
    mutate(
      Occ = case_when(
        A_MJOCC==1~"Busn", A_MJOCC==2~"Prof", A_MJOCC==3~"Serv",
        A_MJOCC==4~"Sale", A_MJOCC==5~"Admn", A_MJOCC==6~"Farm",
        A_MJOCC==7~"Cons", A_MJOCC==8~"Main", A_MJOCC==9~"Prod",
        A_MJOCC==10~"Tran", A_MJOCC==11~"Mili", TRUE~"NA")
    )
  
  cpsmar_e <- merge(hhcps, cpsmar_e, by.x="H_SEQ") %>%
    rename(region=GEREG, county=GTCO) %>%
    mutate(city = if_else(GTCBSAST %in% c(1,2), 1, 0)) %>%
    group_by(H_SEQ) %>%
    mutate(child_u6 = as.integer(any(age < 6))) %>%
    ungroup() %>%
    filter(fulltime == 1) %>%
    select(age, female, hisp, HSGrad, SomeColl, CollDeg,
           earnings, hours, weeks, union, uncov, region,
           race, marital, fulltime, county, city, Occ, child_u6, H_SEQ)
  
  write_csv(cpsmar_e, file.path(outdir, "cpsmar_e.csv"))
}

# Loop over years 2019–2024
for (yr in 2019:2024) {
  process_cps_asec(yr)
}

#-------------------------------------------------------------------------------

