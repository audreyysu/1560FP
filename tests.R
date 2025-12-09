# Tiny sanity checks 
# Source this file after sourcing other scripts.

test_parsing <- function() {
  df <- data.frame(
    Date = "2024-05-01",
    `Driver.ID` = "DRV-1",
    Route = 10, Trip = 123, Stop = "conv", Stop.Sequence = 1,
    `Scheduled.Time` = "2024-05-01 07:00:00",
    `Actual.Arrival.Time` = "2024-05-01 07:04:30",
    Delay.Sec = NA_real_,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  cl <- clean(df)
  stopifnot(inherits(cl$sched, "POSIXct"))
  stopifnot(inherits(cl$actual, "POSIXct"))
  stopifnot(is.finite(cl$Delay.Sec[1]))
  TRUE
}

test_stop_summary <- function() {
  df <- data.frame(
    Date = rep("2024-05-01", 4),
    `Driver.ID` = "D",
    Route = c(10,10,10,10),
    Trip  = c(1,1,2,2),
    Stop  = c("A","A","A","B"),
    Stop.Sequence = c(1,2,1,1),
    `Scheduled.Time` = rep("2024-05-01 07:00:00", 4),
    `Actual.Arrival.Time` = c("2024-05-01 07:03:00","2024-05-01 07:06:00",
                              "2024-05-01 07:02:00","2024-05-01 07:20:00"),
    Delay.Sec = c(180,360,120,1200),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  cl <- clean(df)
  ss <- summarize_by_stop(cl, threshold_sec = 300)
  stopifnot(all(c("Stop","Route","arrivals","ontime_rate") %in% names(ss)))
  TRUE
}

