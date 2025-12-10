#' Simple base-R plots for delay diagnostics
#' @param tbl cleaned data and route integer 
#' @param stratify TRUE/FALSE to stratify by weather or just do overall
#' @param trim to remove outliers from plots

library(dplyr)
library(ggplot2)

.clip_to_quantiles <- function(x, lo = 0.01, hi = 0.99) {
  x <- x[is.finite(x)]
  qs <- quantile(x, c(lo, hi), na.rm = TRUE)
  pmin(pmax(x, qs[1]), qs[2])
}

.plot_win <- function(x, lo = 0.01, hi = 0.99) {
  .clip_to_quantiles(x, lo, hi)
}

# Histogram of delay
plot_delay_histogram <- function(tbl, route = NULL, trim = c(0.01, 0.99), stratify = TRUE) {
  if (stratify) {
    tbl <- tbl[tbl$weather_cat %in% c("clear", "precip"), ]
    groups <- unique(tbl$weather_cat)
  } else {
    groups <- "all"
    tbl$weather_cat <- "all"
  }
  
  par(mfrow = c(length(groups), 1), mar = c(4,4,2,1))
  for (w in groups) {
    x <- if (stratify) tbl$Delay.Sec[tbl$weather_cat == w] else tbl$Delay.Sec
    if (!is.null(route)) x <- x[tbl$Route == route]
    x_plot <- .clip_to_quantiles(x, trim[1], trim[2])
    
    hist(x_plot, breaks = 60,
         main = if (stratify) paste("Delay (sec) –", w) else "Delay (sec) – Overall",
         xlab = "Delay (seconds)")
    abline(v = c(-60, 300), col = "red", lty = 2)
  }
  par(mfrow = c(1,1))
}

# Boxplots of delay by hour
plot_delay_by_hour <- function(df, trim = c(0.01, 0.99), stratify = TRUE) {
  
  df <- df %>%
    filter(!is.na(hour) & is.finite(Delay.Sec)) %>%
    mutate(
      Delay.Sec = .plot_win(Delay.Sec, trim[1], trim[2]),
      hour = factor(hour, levels = 0:23)
    )
  
  if (!stratify) {
    df$weather_cat <- "Overall"
  } else {
    df <- df %>% filter(weather_cat %in% c("clear","precip"))
  }
  
  ggplot(df, aes(x = hour, y = Delay.Sec, fill = weather_cat)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
    labs(
      title = if (stratify) "Delay by Hour and Weather Category" else "Delay by Hour – Overall",
      x = "Hour of Day",
      y = "Delay (seconds)",
      fill = "Weather"
    ) +
    theme_minimal() +
    theme(legend.position = if (stratify) "top" else "none")
}

# Median + IQR per hour
plot_delay_quantiles_by_hour_weather <- function(tbl, ylim_top = NULL, stratify = TRUE) {
  
  if (!stratify) {
    tbl$weather_cat <- "Overall"
  } else {
    tbl <- tbl[tbl$weather_cat %in% c("clear","precip"), ]
  }
  
  weathers <- unique(tbl$weather_cat)
  
  # black for Overall, blue/orange for clear/precip
  colors <- if (!stratify) {
    "black"
  } else {
    c("blue","orange")[seq_along(weathers)]
  }
  
  hrs <- 0:23
  med_mat <- q1_mat <- q3_mat <- matrix(NA_real_,
                                        nrow = length(hrs), ncol = length(weathers),
                                        dimnames = list(hrs, weathers)
  )
  
  for (i in seq_along(weathers)) {
    d <- tbl[tbl$weather_cat == weathers[i], ]
    d <- d[!is.na(d$hour) & is.finite(d$Delay.Sec),
           c("hour","Delay.Sec")]
    d$Delay.Sec <- .plot_win(d$Delay.Sec)
    
    for (h in hrs) {
      x <- d$Delay.Sec[d$hour == h]
      if (length(x)) {
        med_mat[as.character(h), weathers[i]] <- median(x, na.rm = TRUE)
        q1_mat[as.character(h), weathers[i]]  <- quantile(x, 0.25, na.rm = TRUE)
        q3_mat[as.character(h), weathers[i]]  <- quantile(x, 0.75, na.rm = TRUE)
      }
    }
  }
  
  y_min <- min(q1_mat, med_mat, q3_mat, na.rm = TRUE)
  y_max <- max(q1_mat, med_mat, q3_mat, na.rm = TRUE)
  if (!is.null(ylim_top) && is.finite(ylim_top)) y_max <- max(y_max, ylim_top)
  
  plot(hrs, med_mat[,1], type = "n", ylim = c(y_min, y_max),
       xlab = "Hour", ylab = "Delay (seconds)",
       main = if (stratify) "Median delay by hour (with IQR) by weather"
       else "Median delay by hour (with IQR) – Overall")
  abline(h = c(-300, 300), col = "red", lty = 2)
  
  for (i in seq_along(weathers)) {
    lines(hrs, med_mat[,i], type = "b", pch = 16, col = colors[i])
    segments(hrs, q1_mat[,i], hrs, q3_mat[,i], col = colors[i])
  }
  
  legend("topright", legend = weathers,
         col = colors[seq_along(weathers)],
         pch = 16, lty = 1, bty = "n")
}

# Stacked bar: E/O/L by route
plot_route_eol_stacked <- function(route_eol) {
  M <- t(as.matrix(route_eol[, c("early","ontime","late")]))
  op <- par(mar = c(7,4,4,1))
  barplot(M, names.arg = route_eol$Route, las = 2, ylim = c(0,1),
          ylab = "Share", main = "Delay composition by route",
          col = c("green","lightblue","lightpink"))
  
  legend("bottom", inset = c(-0.02, -0.2), xpd = NA, horiz = TRUE,
         fill = c("green","lightblue","lightpink"),
         legend = c("Early","On-time","Late"), bty = "n")
  
  par(op)
}

# Stacked horizontal bars by late share
plot_20_eol_stacked <- function(t20_tbl) {
  M <- t(as.matrix(t20_tbl[, c("early","ontime","late")]))
  labs <- paste0(t20_tbl$Stop, " (R", t20_tbl$Route, ", n=", t20_tbl$n, ")")
  op <- par(mar = c(5,20,4,2))
  
  barplot(M, horiz = TRUE, names.arg = labs, las = 2, xlim = c(0,1),
          xlab = "Share", main = "Best & Worst stops (by late share)",
          col = c("green","lightblue","lightpink"),
          border = NA)
  
  # Adjust legend to sit just above the top bars
  legend("topright", inset = c(0, -0.1), xpd = NA,
         fill = c("green","lightblue","lightpink"),
         legend = c("Early","On-time","Late"), bty = "n")
  
  par(op)
}

# Heat map: on-time share by route and hour combos 
plot_heatmap_route_hour <- function(tbl, threshold_sec = 300) {
  tbl <- tbl[!(tbl$hour %in% c(2L, 3L)), , drop = FALSE]
  
  M <- with(tbl, tapply(as.numeric(abs(Delay.Sec) <= threshold_sec),
                        list(Route = Route, Hour = hour),
                        mean, na.rm = TRUE))
  M[is.nan(M)] <- NA
  
  hrs <- as.numeric(colnames(M))
  rts <- as.numeric(rownames(M))
  
  pal  <- colorRampPalette(c("#f7fbff", "#cfe7f6", "#9cc3e1",
                             "#5a9ecf", "#2b7bb6", "#084c8d"))
  cols <- pal(50)
  
  image(x = hrs, y = rts, z = t(M),
        xlab = "Hour", ylab = "Route",
        main = "On-time share by Route × Hour",
        col = cols)
  box(); grid()
  legend("bottomleft", legend = c("low","high"),
         fill = cols[c(1, length(cols))], bty = "n")
}

# Funnel plot (didn't end up using this later)
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
  p0 <- mean(p, na.rm = TRUE)
  plot(n, p, pch = 16, cex = 0.7, xlab = "Arrivals (n)",
       ylab = "On-time rate", main = "Funnel plot of on-time rate")
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

# Delay by weather

plot_delay_weather <- function(df, lower_bound = -500, trim = c(0.01, 0.99)) {
  
  df <- df %>%
    filter(weather_cat %in% c("clear", "precip")) %>%
    mutate(Delay.Sec = .plot_win(Delay.Sec, trim[1], trim[2]))
  
  ggplot(df, aes(x = weather_cat, y = Delay.Sec, fill = weather_cat)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(lower_bound, NA)) +
    labs(
      title = "Median and IQR of delay by weather category",
      x = "Weather Category",
      y = "Delay (seconds)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}
