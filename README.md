# Gender Wage Gap (2019–2024)

This project analyzes the gender wage gap in the U.S. using CPS ASEC microdata from 2019 to 2024. The analysis was conducted in R and visualized using Power BI. It focuses on full-time workers aged 23 to 62 with positive earnings, exploring how the wage gap evolves under different sets of controls.

## Project Structure

- `2019-2024 Project.Rmd`: Main RMarkdown report
- `cpsmar_2024-2019.R` *(not included)*: Data wrangling and prep script
- `/data` *(not included)*: Contains CPS microdata 
- `/outputs` *(not included)*: Includes tidied CPS and Labor Force csvs for each year
- `Gender Wage Gap Project.pbix`: Interactive Power BI dashboard
- `README.md`: This file

## Key Findings

- A raw gender wage gap of 18% was found among full-time workers aged 23–62 from 2019 to 2024.
- When controlling for education, the gap widened to 25%, reflecting women's higher educational attainment.
- Adding demographic and household factors reduced the gap to 22.7%.
- Among single individuals without young children**, the wage gap narrowed to 14.1%, highlighting the role of caregiving responsibilities.
- Adjusted gaps remained persistent year-over-year (22–24%), with overlapping confidence intervals.
- Over time, women consistently had higher college attainment, while men were more likely to be married and have young children.
- The analysis suggests that even after adjusting for observable traits, a substantial wage gap remains, likely due to structural or unmeasured factors like bias, occupational sorting, and time penalties for caregiving.

## Tools Used

- R (tidyverse, dplyr, ggplot2, haven)
- Power BI (for dashboard visualization)
