#' Run end-to-end: read, clean, summarize, and save outputs
#' @param path_csv character path to otp_simulated.csv, and threshold_sec on-time threshold (default 300sec = +/- 5 min)
#' @return list(invisible) with objects that can be used
run_all <- function(path_csv = "otp_simulated.csv", threshold_sec = 300, save_dir = "results", save_png = TRUE) {
  
  raw <- read_otp_csv(path_csv)
  otp <- clean_otp(raw)
  
  stop_sum <- summarize_by_stop(otp, threshold_sec)
  hour_sum <- summarize_by_route_hour(otp, threshold_sec)
  
  route_eol <- summarize_eol_by_route(otp, threshold_sec)
  stop_eol  <- summarize_eol_by_stop_route(otp, threshold_sec)
  worst20E  <- worst20_by_late(stop_eol, min_n = 30L)  # <- min n filter
  
  
  #Removed for recursion but here in case needed to replot:
  
  # E/O/L by route (stacked bars)
#  plot_route_eol_stacked(res$route_eol)
  # Worst 20 stop–route combos ranked by LATE share (stacked bars)
  #w20 <- worst20_by_late(res$stop_eol)
  #plot_worst20_eol_stacked(w20)
  # HEATMAP
  #plot_heatmap_route_hour(res$otp, threshold_sec = 300)
  # MEDIAN + IQR BY HOUR
#  plot_delay_quantiles_by_hour(res$otp)
  # FUNNEL (guards against small-n false alarms)
#  plot_funnel_ontime(res$stop_sum, conf = 0.95)

  
  
    invisible(list(
    otp = otp,
    stop_sum = stop_sum,
    hour_sum = hour_sum,
    route_eol = route_eol,
    stop_eol = stop_eol,
    worst20_by_late = worst20E
  ))
}

