#' Simple base-R plots for delay diagnostics
#' @param tbl cleaned data and route integer 

.clip_to_quantiles <- function(x, lo = 0.01, hi = 0.99) {
  x <- x[is.finite(x)]
  qs <- quantile(x, c(lo, hi), na.rm = TRUE)
  pmin(pmax(x, qs[1]), qs[2])   # winsorize to [lo, hi]
}

plot_delay_histogram <- function(tbl, route = NULL, trim = c(0.01, 0.99)) {
  x <- tbl$Delay.Sec
  ttl <- "Delay (sec) – all routes"
  if (!is.null(route)) {
    x   <- x[!is.na(tbl$Route) & tbl$Route == route]
    ttl <- paste0("Delay (sec) – Route ", route)
  }
  x_plot <- .clip_to_quantiles(x, trim[1], trim[2])
  rng    <- range(x_plot, na.rm = TRUE)
  
  hist(x_plot, breaks = 60,
       main = ttl, xlab = "Delay (seconds)", xlim = rng)
  abline(v = c(-300, 300), col = "red", lty = 2)
}

#' Boxplots of delay by hour 
plot_delay_by_hour <- function(tbl, trim = c(0.01, 0.99)) {
  d <- tbl[, c("hour","Delay.Sec")]
  d <- d[is.finite(d$Delay.Sec) & !is.na(d$hour), , drop = FALSE]
  
  # force hour labels 0-23 even if some hours have no data
  d$hour <- factor(as.integer(d$hour), levels = 0:23)
  
  # winsorize delays for plotting so points/whiskers don’t explode
  d$Delay.Sec <- .clip_to_quantiles(d$Delay.Sec, trim[1], trim[2])
  
  boxplot(Delay.Sec ~ hour, data = d, outline = FALSE,
          main = "Delay by hour", xlab = "Hour of day", ylab = "Delay (seconds)")
  abline(h = c(-300, 300), col = "red", lty = 2)
}



# Stacked bar: E/O/L by route
plot_route_eol_stacked <- function(route_eol) {
  M <- t(as.matrix(route_eol[, c("early","ontime","late")]))
  op <- par(mar = c(7,4,4,1))
  barplot(M, names.arg = route_eol$Route, las = 2, ylim = c(0,1),
          ylab = "Share", main = "Delay composition by route",
          col = c("green","lightblue","lightpink"))
  legend("topright", inset = c(0, -0.2), xpd=NA,
         fill = c("green","lightblue","lightpink"),
         legend = c("Early","On-time","Late"), bty = "n")
  par(op)
}

# Stacked horizontal bars: worst 20 by LATE share 
plot_worst20_eol_stacked <- function(worst20_tbl) {
  M <- t(as.matrix(worst20_tbl[, c("early","ontime","late")]))
  labs <- paste0(worst20_tbl$Stop, " (R", worst20_tbl$Route, ", n=", worst20_tbl$n, ")")
  op <- par(mar = c(5,20,4,2))
  barplot(M, horiz = TRUE, names.arg = labs, las = 2, xlim = c(0,1),
          xlab = "Share", main = "Worst 20 stops (by late share)",
          col = c("green","lightblue","lightpink"))
  legend("topright", inset = c(0, -0.15), xpd=NA,
         fill = c("green","lightblue","lightpink"),
         legend = c("Early","On-time","Late"), bty = "n")
  par(op)
}


# Heatmap: on-time share by Route x Hour 
plot_heatmap_route_hour <- function(tbl, threshold_sec = 300) {
  tbl <- tbl[!(tbl$hour %in% c(2L, 3L)), , drop = FALSE]
  
  M <- with(tbl, tapply(as.numeric(abs(Delay.Sec) <= threshold_sec),
                        list(Route = Route, Hour = hour),
                        function(z) mean(z, na.rm = TRUE)))
  M[is.nan(M)] <- NA
  hrs <- as.numeric(colnames(M))
  rts <- as.numeric(rownames(M))
  
  pal  <- colorRampPalette(c("#f7fbff", "#cfe7f6", "#9cc3e1", "#5a9ecf", "#2b7bb6", "#084c8d"))
  cols <- pal(50)
  
  image(x = hrs, y = rts, z = t(M),
        xlab = "Hour", ylab = "Route",
        main = "On-time share by Route × Hour",
        col = cols)               # ← removed useRaster=TRUE
  box(); grid()
  legend("bottomleft", legend = c("low","high"),
         fill = cols[c(1, length(cols))], bty = "n")
}

# Median and IQR by hour
plot_delay_quantiles_by_hour <- function(tbl, ylim_top = NULL) {
  d <- tbl[!is.na(tbl$hour) & is.finite(tbl$Delay.Sec), c("hour","Delay.Sec")]
  d$Delay.Sec <- .plot_win(d$Delay.Sec)     
  
  hrs <- 0:23
  med <- q1 <- q3 <- rep(NA_real_, length(hrs))
  for (i in seq_along(hrs)) {
    x <- d$Delay.Sec[d$hour == hrs[i]]
    if (length(x)) {
      med[i] <- median(x, na.rm = TRUE)
      q1[i]  <- quantile(x, 0.25, na.rm = TRUE, names = FALSE)
      q3[i]  <- quantile(x, 0.75, na.rm = TRUE, names = FALSE)
    }
  }
  
  # y-limits: raise the top if pass ylim_top
  y_min <- min(q1, med, q3, na.rm = TRUE)
  y_max <- max(q1, med, q3, na.rm = TRUE)
  if (!is.null(ylim_top) && is.finite(ylim_top)) y_max <- max(y_max, ylim_top)
  
  plot(hrs, med, type = "b", pch = 16, xlab = "Hour",
       ylab = "Delay (seconds)",
       main = "Median delay by hour with IQR",
       ylim = c(y_min, y_max))
  segments(hrs, q1, hrs, q3)
  abline(h = c(-300, 300), col = "red", lty = 2)
}

.plot_win <- function(x, lo = 0.01, hi = 0.99) .clip_to_quantiles(x, lo, hi)


# Wilson CI for binomial proportion
.wilson <- function(k, n, conf = 0.95) {
  if (n == 0) return(c(NA, NA))
  z  <- qnorm(1 - (1 - conf)/2)
  ph <- k/n
  denom <- 1 + z^2/n
  center <- (ph + z^2/(2*n)) / denom
  half   <- z * sqrt((ph*(1-ph)/n) + (z^2/(4*n^2))) / denom
  c(center - half, center + half)
}

plot_funnel_ontime <- function(stop_summary, conf = 0.95) {
  n <- stop_summary$arrivals
  p <- stop_summary$ontime_rate
  # reference line = global on-time
  p0 <- mean(p, na.rm = TRUE)
  plot(n, p, pch = 16, cex = 0.7, xlab = "Arrivals (n)",
       ylab = "On-time rate", main = "Funnel plot of on-time rate")
  # draw Wilson bands across a smooth n-grid
  nn <- pretty(range(n, na.rm = TRUE), n = 100)
  low <- high <- rep(NA_real_, length(nn))
  for (i in seq_along(nn)) {
    ci <- .wilson(round(p0*nn[i]), nn[i], conf = conf)
    low[i]  <- ci[1]
    high[i] <- ci[2]
  }
  lines(nn, low,  lty = 2)
  lines(nn, high, lty = 2)
  abline(h = p0, col = "grey40")
}


