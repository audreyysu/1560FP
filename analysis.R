source("clean.R")

#' Stop-level reliability summary
#'
#' @param tbl cleaned table from clean_otp()
#' @param threshold_sec on-time threshold in seconds (default 300)
#' @return data.frame with Stop, Route, arrivals, ontime_rate, median_delay, p90_delay, mean_delay
summarize_by_stop <- function(tbl, threshold_sec = 300) {
  ok <- !is.na(tbl$Stop) & !is.na(tbl$Route)
  d  <- tbl[ok, , drop = FALSE]
  
  # Split by Stop and Route
  key <- paste(d$Stop, d$Route, sep = "||")
  idx <- split(seq_len(nrow(d)), key)
  
  res <- lapply(idx, function(ii) {
    delays <- d$Delay.Sec[ii]
    n      <- length(ii)
    ot     <- mean(is_on_time(delays, threshold_sec = threshold_sec), na.rm = TRUE)
    data.frame(
      Stop = d$Stop[ii[1]],
      Route = d$Route[ii[1]],
      arrivals = n,
      ontime_rate = ot,
      median_delay = safe_median(delays),
      p90_delay = safe_quant(delays, 0.9),
      mean_delay = mean(delays, na.rm = TRUE),
      row.names = NULL
    )
  })
  do.call(rbind, res)
}

#' Route-hour summary (reliability by time-of-day)
#'
#' @param tbl cleaned table
#' @param threshold_sec seconds
#' @return data.frame Route,hour,arrivals,ontime_rate,median_delay
summarize_by_route_hour <- function(tbl, threshold_sec = 300) {
  d <- tbl[!is.na(tbl$Route) & !is.na(tbl$hour), , drop = FALSE]
  key <- paste(d$Route, d$hour, sep = "||")
  idx <- split(seq_len(nrow(d)), key)
  res <- lapply(idx, function(ii) {
    delays <- d$Delay.Sec[ii]
    data.frame(
      Route = d$Route[ii[1]],
      hour = d$hour[ii[1]],
      arrivals = length(ii),
      ontime_rate = mean(is_on_time(delays, threshold_sec), na.rm = TRUE),
      median_delay = safe_median(delays),
      p90_delay = safe_quant(delays, 0.9),
      row.names = NULL
    )
  })
  do.call(rbind, res)
}


# Early/On-time/Late classifier helper function
classify_eol <- function(delay_sec, threshold_sec = 300) {
  early  <- delay_sec < -threshold_sec
  late   <- delay_sec >  threshold_sec
  ontime <- !early & !late & !is.na(delay_sec)
  data.frame(
    early  = as.numeric(early),
    ontime = as.numeric(ontime),
    late   = as.numeric(late)
  )
}

# E/O/L proportions by Route by route only (see documentation below)
summarize_eol_by_route <- function(tbl, threshold_sec = 300) {
  eol <- classify_eol(tbl$Delay.Sec, threshold_sec)
  out <- aggregate(eol, by = list(Route = tbl$Route),
                   FUN = function(x) mean(x, na.rm = TRUE))
  # fix order
  out <- out[, c("Route","early","ontime","late")]
  out
}

#' Summarize Stop–Route Early/On-time/Late (E/O/L) Proportions
#'
#' @param tbl df_weather
#' @param threshold_sec numeric threshold (5 mins) for lateness
#' @return df with one row per stop-route combination
#' @example
#' stop_route_summary <- summarize_eol_by_stop_route(otp_data)
summarize_eol_by_stop_route <- function(tbl, threshold_sec = 300) {
  key <- paste(tbl$Stop, tbl$Route, sep = "||")
  idx <- split(seq_len(nrow(tbl)), key)
  res <- do.call(rbind, lapply(names(idx), function(k) {
    ii  <- idx[[k]]
    eol <- classify_eol(tbl$Delay.Sec[ii], threshold_sec)
    data.frame(
      Stop = tbl$Stop[ii[1]],
      Route = tbl$Route[ii[1]],
      n = length(ii),
      early = mean(eol$early,  na.rm = TRUE),
      ontime = mean(eol$ontime, na.rm = TRUE),
      late = mean(eol$late,   na.rm = TRUE),
      row.names = NULL
    )
  }))
  res
}

#' Select Worst 20 Stops by Late Share
#'
#' @param stop_route_eol df that must have columns `late`, `ontime`, and `n`.
#' @param min_n min obs for stop to be considered, default 30
#'
#' @return df w <=20 rows corresponding to the stops with the highest late share
#'
#' @example
#' worst_stops <- worst20_by_late(stop_route_eol)

worst20_by_late <- function(stop_route_eol, min_n = 30L) {
  d <- stop_route_eol[stop_route_eol$n >= min_n, , drop = FALSE]
  if (!nrow(d)) return(d)
  o <- order(-d$late, d$ontime)
  d[o[seq_len(min(20, nrow(d)))], , drop = FALSE]
}