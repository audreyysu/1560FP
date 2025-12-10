# workflow.R

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)
library(openmeteo)

run_workflow <- function(raw_file_path, sample_size = 5000, seed = 1560) {
  # all source scripts
  source("clean.R")
  source("weather.R")
  source("model.R") 
  source("plotting.R")
  source("analysis.R")
  
  # load, clean data (clean.R)
  raw_df <- read.csv(raw_file_path, stringsAsFactors = FALSE)
  
  message("Cleaning data...")
  df <- clean(raw_df)
  
  # add weather data (openmeteo package, weather.R)
  message("Adding weather data (expect 1-2 mins)...")
  df_weather <- df %>%
    snap_to_grid() %>%
    fetch_weather()
  
  message("Preview of data:")
  print(head(df_weather, 10))
  
  # summary stats (analysis.R)
  message("Stop-level and route-level summaries...")
  stop_summary <- summarize_by_stop(df_weather)
  route_eol <- summarize_eol_by_route(df_weather)
  stop_route_eol <- summarize_eol_by_stop_route(df_weather, min_n = 30)
  
  # 10 best & 10 worst stops by LATE share
  worst10 <- stop_route_eol %>% slice_max(late, n = 10, with_ties = FALSE)
  best10  <- stop_route_eol %>% slice_min(late, n = 10, with_ties = FALSE)
  best_worst20 <- bind_rows(best10, worst10)
  
  # model fitting (model.R)
  message("Fitting weighted GLM and GLMM...")
  model_results <- analyze_lateness(df_weather, sample_size = sample_size, seed = seed)
  
  message("GLM Summary:")
  print(summary(model_results$model_glm))
  
  message("GLMM Summary:")
  print(summary(model_results$model_glmer))
  
  # plots (plotting.R)
  message("Generating plots...")
  
  if (!dir.exists("visualizations")) dir.create("visualizations")
  
  save_plot <- function(plot_obj, filename, width = 8, height = 5) {
    if (!dir.exists("visualizations")) dir.create("visualizations")
    ggsave(filename = file.path("visualizations", filename),
           plot = plot_obj, width = width, height = height)
  }
  
  png("visualizations/route_eol_stacked.png", width = 800, height = 600)
  plot_route_eol_stacked(route_eol)
  dev.off()
  
  png("visualizations/20_eol_stacked.png", width = 1000, height = 600)
  plot_20_eol_stacked(best_worst20)
  dev.off()
  
  png("visualizations/delay_histogram_all.png", width = 800, height = 600)
  plot_delay_histogram(df_weather, stratify = FALSE)
  dev.off()
  
  png("visualizations/delay_histogram_weather.png", width = 800, height = 600)
  plot_delay_histogram(df_weather, stratify = TRUE)
  dev.off()
  
  png("visualizations/delay_quantiles_all.png", width = 800, height = 600)
  plot_delay_quantiles_by_hour_weather(df_weather, stratify = FALSE)
  dev.off()
  
  png("visualizations/delay_quantiles_weather.png", width = 800, height = 600)
  plot_delay_quantiles_by_hour_weather(df_weather, stratify = TRUE)
  dev.off()
  
  png("visualizations/heatmap_route_hour.png", width = 800, height = 600)
  plot_heatmap_route_hour(df_weather)
  dev.off()
  
  # ggplot objects
  p_delay_hour <- plot_delay_by_hour(df_weather, stratify = TRUE)
  ggsave("visualizations/delay_by_hour.png", plot = p_delay_hour, width = 8, height = 5)
  
  p_delay_weather <- plot_delay_weather(df_weather)
  ggsave("visualizations/delay_by_weather.png", plot = p_delay_weather, width = 8, height = 5)
  
  # model.R plots (already ggplot)
  save_plot(model_results$plot_glm, "model_glm_pred_late.png")
  save_plot(model_results$plot_glm_vs_glmer, "model_glm_vs_glmer_pred_late.png")
  
  # results list
  list(
    df_weather = df_weather,
    stop_summary = stop_summary,
    route_eol = route_eol,
    stop_route_eol = stop_route_eol,
    best_worst20 = best_worst20,
    model_results = model_results
  )
}



############ USAGE
# results <- run_workflow("otp_simulated.csv", sample_size = 5000)
# 
# # preview cleaned data
# head(results$df_weather)
# 
# # stop summaries
# head(results$stop_summary)
# 
# # model results
# summary(results$model_results$model_glm)
# summary(results$model_results$model_glmer)
