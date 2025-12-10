#' CLEAN.R - READING otp_simulated.csv
#' 5 FUNCTIONS: read_data(), clean(), is_on_time(), safe_median(), safe_quant()

#' Read in your data (.csv)
#' 
#' @param file_path .csv 
#' @example df <- read_data("otp_simulated.csv")
read_data <- function(file_path) {
  read.csv(file_path, stringsAsFactors = FALSE)
}

#' Clean/parse OTP table and fix timing
#'
#' @param x data.frame as read by read_otp_csv()
#' @return data.frame with columns:
#'   Date (Date), Driver.ID (chr), Route (int), Trip (int),
#'   Stop (chr), Stop.Sequence (int),
#'   Scheduled.Time (POSIXct), Actual.Arrival.Time (POSIXct),
#'   Rounded.Time (POSIXct), Delay.Sec (numeric), early, late, ontime
clean <- function(x) {
  # fix time columns
  out <- x %>%
    mutate(
      Date = as.POSIXct(Date, format = "%Y-%m-%d", tz = "UTC"),
      Scheduled.Time = as.POSIXct(Scheduled.Time,
                                  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      Actual.Arrival.Time = lubridate::ymd_hms(Actual.Arrival.Time, tz = "UTC"),
      Rounded.Time = lubridate::floor_date(Scheduled.Time, "hour")
    )
  
  # convert to integer
  out$Route         <- as.integer(out$Route)
  out$Trip          <- as.integer(out$Trip)
  out$Stop.Sequence <- as.integer(out$Stop.Sequence)
  out$hour <- as.integer(format(out$Scheduled.Time, "%H"))
  
  # compute raw delay (seconds) and account for extreme values
  d <- as.numeric(difftime(out$Actual.Arrival.Time, out$Scheduled.Time, units = "secs"))
  if ("Delay.Sec" %in% names(out)) {
    ds  <- suppressWarnings(as.numeric(out$Delay.Sec))
    bad <- !is.finite(ds) | abs(ds) > 12 * 3600
    ds[bad] <- d[bad]
    d <- ds
  }
  d <- ((d + 12*3600) %% (24*3600)) - 12*3600
  
  out$Delay.Sec <- d
  
  # late = >5 min late
  # early = >1 min early
  # on-time = anything in between
  out <- out %>%
    mutate(
      early  = Delay.Sec < -60,
      late   = Delay.Sec > 300,
      ontime = !early & !late
    )
  
  return(out)
}


#' Safe median (to make plots more readable)
safe_median <- function(z) {
  if (length(z)) median(z, na.rm = TRUE) else NA_real_
}

#' Safe quantile p (some outliers make the plots unreadable)
safe_quant <- function(z, p = 0.9) {
  if (length(z)) as.numeric(quantile(z, probs = p, na.rm = TRUE, names = FALSE)) else NA_real_
}
