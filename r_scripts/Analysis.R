# ---------------------------------------------------------------------------
# Source file:  Analysis.R
# Data files:   cpsmar_e.csv   (written by Wrangle.R)
# Output files: gaps_by_spec.csv, gap_trend.csv
#               trend_2019_2024.png, gaps_by_spec.png,
#               distribution.png
# Content:      Builds the analysis sample, fits sequential specs, computes
#               year-by-year adjusted gaps, and saves key figures/tables.
# Author:       Bradley DePanfilis
# Last updated: 10 Aug 2025
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(broom)
  library(ggplot2)
  library(scales)
  library(here)
  library(sandwich)   
  library(lmtest)    
})

# ---------- setup ----------
dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("dashboard"),         recursive = TRUE, showWarnings = FALSE)

years <- 2019:2024

# ---------- load wrangled per-year files ----------
cps_list <- lapply(years, function(y) {
  f <- here("outputs", as.character(y), "cpsmar_e.csv")
  if (!file.exists(f)) stop("Missing file: ", f)
  d <- read_csv(f, show_col_types = FALSE)
  d$year <- y
  d
})
cpsmar_e <- bind_rows(cps_list)

# ---------- build analysis sample ----------
cpsmar_a <- cpsmar_e %>%
  filter(age >= 23, age <= 62, earnings > 0) %>%
  mutate(
    wage         = earnings / (hours * weeks),
    lwage        = log(wage),
    age_centered = age - 23,
    Black        = if_else(race == 2, 1, 0),
    south        = if_else(region == 3, 1, 0),
    married      = if_else(marital %in% c(1, 2, 3), 1, 0),
    female       = as.integer(female == 1)
  ) %>%
  filter(is.finite(lwage), wage > 0)

# Singles subset
singles <- cpsmar_a %>% filter(married == 0, child_u6 == 0)

# ---------- helpers ----------
# % gap as in the Rmd (100 * beta) with OLS CIs
coef_to_pct_ols <- function(fit, term = "female") {
  broom::tidy(fit, conf.int = TRUE) |>
    dplyr::filter(term == !!term) |>
    dplyr::transmute(
      gap_pct = 100 * estimate,
      ci_low  = 100 * conf.low,
      ci_high = 100 * conf.high
    )
}

# Robust HC1 version for spec table (adds robust SE)
coef_to_pct_robust <- function(fit, term = "female") {
  V   <- sandwich::vcovHC(fit, type = "HC1")
  ct  <- lmtest::coeftest(fit, vcov. = V)
  est <- ct[term, "Estimate"]
  se  <- ct[term, "Std. Error"]
  ci_low  <- est - 1.96 * se
  ci_high <- est + 1.96 * se
  tibble(
    gap_pct = 100 * est,
    ci_low  = 100 * ci_low,
    ci_high = 100 * ci_high,
    se_pct  = 100 * se
  )
}

# ---------- sequential specs  ----------
m_baseline <- lm(lwage ~ female + age_centered + I(age_centered^2), data = cpsmar_a)
m_edu      <- lm(lwage ~ female + age_centered + I(age_centered^2) + HSGrad + SomeColl + CollDeg, data = cpsmar_a)
m_person   <- lm(lwage ~ female + age_centered + I(age_centered^2) + HSGrad + SomeColl + CollDeg + Black + hisp + south + city, data = cpsmar_a)
m_house    <- lm(lwage ~ female + age_centered + I(age_centered^2) + HSGrad + SomeColl + CollDeg + Black + hisp + south + city + married + child_u6, data = cpsmar_a)
m_singles  <- lm(lwage ~ female + age_centered + I(age_centered^2) + HSGrad + SomeColl + CollDeg + Black + hisp + south + city, data = singles)

# Robust (HC1) table for spec comparison
spec_table <- bind_rows(
  tibble(spec = "Baseline")      %>% bind_cols(coef_to_pct_robust(m_baseline)),
  tibble(spec = "Add Education") %>% bind_cols(coef_to_pct_robust(m_edu)),
  tibble(spec = "Add Person")    %>% bind_cols(coef_to_pct_robust(m_person)),
  tibble(spec = "Add Household") %>% bind_cols(coef_to_pct_robust(m_house)),
  tibble(spec = "Only Singles")  %>% bind_cols(coef_to_pct_robust(m_singles))
)
write_csv(spec_table, here("outputs", "tables", "gaps_by_spec.csv"))

# ---------- year-by-year adjusted gap (full spec = m_house) ----------
get_year_coef <- function(d) {
  fit <- lm(lwage ~ female + age_centered + I(age_centered^2) +
              HSGrad + SomeColl + CollDeg +
              Black + hisp + south + city +
              married + child_u6, data = d)
  coef_to_pct_ols(fit)   # OLS, to match the Rmd trend
}
trend <- cpsmar_a %>% group_by(year) %>% group_modify(~ get_year_coef(.x)) %>% ungroup()
write_csv(trend, here("outputs", "tables", "gap_trend.csv"))

# ---------- figures  ----------
theme_set(theme_minimal(base_size = 14))
light <- theme(
  plot.background  = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.grid.major = element_line(colour = "#e5e7eb"),
  panel.grid.minor = element_blank(),
  plot.title       = element_text(face = "bold", size = 18, colour = "#111827"),
  axis.text        = element_text(colour = "#111827"),
  axis.title       = element_text(colour = "#111827")
)

out_dir <- "dashboard"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
save_plot <- function(p, name, w, h, dpi = 300) {
  fn <- file.path(out_dir, name)
  ggsave(fn, p, width = w, height = h, dpi = dpi, bg = "white")
  message("✓ Wrote: ", normalizePath(fn))
}

# --- Spec bars: side-by-side gap (green, negative) + robust SE (light-blue, positive) ---
spec_levels <- c("Baseline","Add Education","Add Person","Add Household","Only Singles")
spec_table  <- spec_table %>% mutate(spec = factor(spec, levels = spec_levels))

spec_plot <- spec_table %>%
  mutate(
    gap_dec = gap_pct / 100,   # e.g., -0.250
    se_dec  = se_pct  / 100    # e.g.,  0.004
  )

spec_long <- spec_plot %>%
  select(spec, gap_dec, se_dec) %>%
  pivot_longer(c(gap_dec, se_dec), names_to = "Metric", values_to = "value") %>%
  mutate(Metric = recode(Metric,
                         gap_dec = "Female Wage Gap",
                         se_dec  = "Robust Standard Error"),
         Metric = factor(Metric, levels = c("Female Wage Gap","Robust Standard Error")))

pd <- position_dodge(width = 0.60)
col_gap <- "#14b8a6"  # green
col_se  <- "#93c5fd"  # light blue
col_se_text <- "#2563eb"

p_spec <- ggplot(spec_long, aes(x = spec, y = value, fill = Metric)) +
  geom_col(width = 0.55, position = pd) +
  geom_text(
    data = filter(spec_long, Metric == "Robust Standard Error"),
    aes(label = sprintf("%.3f", value)),
    colour = col_se_text, size = 4, vjust = -0.4, position = pd
  ) +
  scale_fill_manual(values = c("Female Wage Gap" = col_gap,
                               "Robust Standard Error" = col_se)) +
  scale_y_continuous(limits = c(-0.26, 0.006),
                     breaks = seq(-0.26, 0, 0.02),
                     labels = label_number(accuracy = 0.01)) +
  labs(title = "Female Wage Gap and Robust Standard Error by Model",
       x = "Model", y = "Gap / SE (decimal)", fill = NULL) +
  theme_minimal(base_size = 14) + light +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 12, hjust = 1))
save_plot(p_spec, "gaps_by_spec.png", 12, 6)

# --- Distribution: white bg + dashed means + right-side $ labels ---
cpsmar_a <- cpsmar_a %>% mutate(gender = if_else(female == 1, "Female", "Male"))

earnings_avg_year <- cpsmar_a %>%
  group_by(year, gender) %>%
  summarise(avg_earnings = mean(earnings, na.rm = TRUE), .groups = "drop")

max_density_y <- cpsmar_a %>%
  group_by(year) %>%
  summarise(y_max = max(density(earnings, na.rm = TRUE)$y), .groups = "drop")

gap_by_year <- earnings_avg_year %>%
  pivot_wider(names_from = gender, values_from = avg_earnings) %>%
  mutate(gap = Male - Female)

x_limit <- 400000
earnings_avg_year <- earnings_avg_year %>%
  left_join(max_density_y, by = "year") %>%
  left_join(gap_by_year %>% select(year, gap), by = "year") %>%
  mutate(
    gap_scaled = pmax(rescale(gap, to = c(0.1, 0.4)), 0.25),
    label_x    = x_limit * 0.98,
    label_y    = if_else(gender == "Female",
                         y_max * (0.70 + gap_scaled/2),
                         y_max * (0.70 - gap_scaled/2)),
    label      = dollar(round(avg_earnings, 0))
  )

p_dist <- ggplot(cpsmar_a, aes(earnings, fill = gender)) +
  geom_density(alpha = 0.40, position = "identity", colour = "black", linewidth = 0.35) +
  geom_vline(data = earnings_avg_year,
             aes(xintercept = avg_earnings, colour = gender),
             linetype = "dashed", linewidth = 0.9, show.legend = FALSE) +
  geom_text(data = earnings_avg_year,
            aes(x = label_x, y = label_y, label = label, colour = gender),
            hjust = 1, size = 3.3, show.legend = FALSE) +
  scale_fill_manual(values = c("Female"="#e78ac3","Male"="#4eb3d3")) +
  scale_colour_manual(values = c("Female"="#e78ac3","Male"="#4eb3d3")) +
  scale_x_continuous(labels = label_dollar(),
                     limits = c(0, x_limit), breaks = seq(0, x_limit, 100000)) +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "Distribution of Earnings by Gender and Year (2019–2024)",
       subtitle = "Dashed lines show average earnings; vertical spacing reflects earnings gap",
       x = "Earnings", y = "Density", fill = "Gender") +
  theme_minimal(base_size = 14) + light +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        legend.position = "right")
save_plot(p_dist, "distribution.png", 12, 6)

# --- Trend (OLS CIs) ---
p_trend <- ggplot(trend, aes(year, gap_pct)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.18, fill = "#f9a8d4") +
  geom_line(linewidth = 1, colour = "#db2777") +
  geom_point(size = 2.6, colour = "#db2777") +
  scale_x_continuous(breaks = sort(unique(trend$year))) +
  labs(title = "Estimated Female Wage Gap with Controls (2019–2024)",
       x = "Year", y = "Gap (percentage points)") +
  theme_minimal(base_size = 14) + light
save_plot(p_trend, "trend_2019_2024.png", 10, 6)

message("✓ Analysis complete: outputs/tables/{gaps_by_spec.csv,gap_trend.csv} and dashboard/*.png written.")
