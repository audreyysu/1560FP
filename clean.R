
read_otp_csv <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Clean/parse OTP table and fix timing
#'
#' @param x data.frame as read by read_otp_csv()
#' @return data.frame with columns:
#'   Date (Date), Driver.ID (chr), Route (int), Trip (int),
#'   Stop (chr), Stop.Sequence (int),
#'   sched (POSIXct), actual (POSIXct),
#'   Delay.Sec (numeric), hour (int)
clean_otp <- function(x) {
  out <- x
  #small check
  out$Route <- as.integer(out$Route)
  out$Trip <- as.integer(out$Trip)
  out$Stop.Sequence <- as.integer(out$Stop.Sequence)
  
  ## get times
  fmt <- "%Y-%m-%d %H:%M:%S"
  tz_used <- "America/New_York"   #probably not necessary but sets timezone in case
  
  out$sched  <- as.POSIXct(strptime(out[["Scheduled.Time"]], fmt, tz = tz_used))
  out$actual <- as.POSIXct(strptime(out[["Actual.Arrival.Time"]], fmt, tz = tz_used))
  
  # Derive date/hour from 'scheduled' time
  out$Date <- as.Date(out$sched, tz = tz_used)
  out$hour <- as.integer(format(out$sched, "%H"))
  
  
  # raw difference in seconds:
  d <- as.numeric(difftime(out$actual, out$sched, units = "secs"))
  
  # remove if not needed, but has delay check in case
  if ("Delay.Sec" %in% names(out)) {
    ds <- as.numeric(out$Delay.Sec)
    # replace missing or absurd values with recomputed:
    bad <- !is.finite(ds) | abs(ds) > 12*3600
    ds[bad] <- d[bad]
    d <- ds
  }
  
  # move to the closest 24h equivalent: [-12h, +12h]
  d <- ((d + 12*3600) %% (24*3600)) - 12*3600
  
  out$Delay.Sec <- d
  
  # Keep the columns used later
  keep <- c("Date","Driver.ID","Route","Trip","Stop","Stop.Sequence",
            "sched","actual","Delay.Sec","hour")
  out[keep]
}


#' Binary on-time classifier
#'
#' @param delay_sec numeric vector of delays (seconds; negative = early)
#' @param threshold_sec nonnegative scalar; default 300 (=5 minutes)
#' @return logical vector: TRUE if |delay_sec| <= threshold, else FALSE
is_on_time <- function(delay_sec, threshold_sec = 300) {
  abs(delay_sec) <= threshold_sec
}

#' Safe median (to make plots more readable)
safe_median <- function(z) {
  if (length(z)) median(z, na.rm = TRUE) else NA_real_
}

#' Safe quantile p (some outliers make the plots unreadable)
safe_quant <- function(z, p = 0.9) {
  if (length(z)) as.numeric(quantile(z, probs = p, na.rm = TRUE, names = FALSE)) else NA_real_
}

