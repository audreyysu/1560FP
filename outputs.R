source("clean.R"); source("analysis.R"); source("plotting.R"); source("run.R")

res <- run_all("otp_simulated.csv", threshold_sec = 300)
print(head(res$stop_sum, 5))


# Inspect in-session
head(res$stop_sum, 10)
head(res$hour_sum, 10)
res$worst20
table(res$otp$hour)       
summary(res$otp$Delay.Sec)  # no ±86400
head(res$worst20, 10)

# Existing helpers
plot_delay_histogram(res$otp)  # overall delay distribution
plot_delay_histogram(res$otp, route = 10)   # distribution for Route 10 only
plot_delay_by_hour(res$otp)  # boxplots of delay by hour
plot_worst20_eol_stacked(res$worst20_by_late)  

#heatmap has some data on hours 2 and 3. Table summary says there should be none,
# and even when i manually tried to remove it it was still there
plot_heatmap_route_hour(res$otp, threshold_sec = 300)
plot_delay_quantiles_by_hour(res$otp)
plot_funnel_ontime(res$stop_sum, conf = 0.95)


#Route-level on-time bar chart
ontime_by_route <- aggregate(
  as.numeric(abs(res$otp$Delay.Sec) <= 300),
  by = list(Route = res$otp$Route),
  FUN = function(z) mean(z, na.rm = TRUE)
)
barplot(ontime_by_route$x, names.arg = ontime_by_route$Route,
        main = "On-time rate by route", ylab = "On-time rate", las = 2)

# E/O/L by route (stacked)
plot_route_eol_stacked(res$route_eol)

