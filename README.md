# OTP Data Analysis Workflow
## PHP 1560 - Audrey Su & Alex Cho

Repo contains complete R workflow for cleaning, summarizing, modeling, analyzing, and visualizing One-Trip-Performance (OTP) data alongside historical weather data (openmeteo). 

---

## Table of Contents

- [Project Structure](#project-structure)  
- [Installation](#installation)  
- [Scripts & Functions](#scripts--functions)  
- [Example Usage](#example-usage) 

---

## Project Structure
```bash
├── workflow.R # Main script to run full analysis
├── clean.R # data cleaning
├── weather.R # weather integration
├── model.R # GLM and GLMM modeling
├── plotting.R # plotting results
├── analysis.R # analyzing stops & routes
├── visualizations/ # folder for plots
├── tests.R # basic tests of functionality
└── otp_simulated.csv # needs to be user-provided .csv
```

---

## Installation

The following R packages are needed:

```r
install.packages(c("dplyr", "lubridate", "lme4", "ggplot2", "tidyr"))
```

## Scripts & Functions
Input must be a .csv with columns `Date	Driver.ID	Route	Trip	Stop	Stop.Sequence	Scheduled.Time	Actual.Arrival.Time	Delay.Sec	StopLat	StopLng`.

- clean.R – contains clean_otp() and helper functions like is_on_time().
- weather.R – adds weather features to the dataset (snap_to_grid(), fetch_weather()).
- analysis.R – summary functions including summarize_by_stop(), summarize_eol_by_route(), summarize_eol_by_stop_route(), worst20_by_late().
- model.R – fits inverse-probability-weighted GLM (glm) and GLMM (glmer, random intercept for driver) models to predict probability of lateness using weather category and hour, with analyze_lateness().
- plotting.R – visualizations, saved to the `visualizations/` folder
  - Delay composition by route (plot_route_eol_stacked())
  - Top/Bottom stops by lateness (plot_20_eol_stacked())
  - Delay histograms (plot_delay_histogram())
  - Delay quantiles by hour and weather (plot_delay_quantiles_by_hour_weather())
  - Delay by hour and weather plots (plot_delay_by_hour(), plot_delay_weather())
  - Heatmaps (plot_heatmap_route_hour())
- tests.R - basic sanity checks

## Example Usage 
(see workflow.R for more details)
```r
results <- run_workflow("otp_simulated.csv", sample_size = 5000)
```
