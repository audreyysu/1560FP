library(dplyr)
library(lubridate)
library(lme4)
library(ggplot2)
library(tidyr)

#' Model Bus Lateness Using Weighted GLM and GLMM
#'
#' Fits weighted logistic regression models (standard GLM and mixed-effects GLMM
#' that incorporates a random intercept per driver) to predict the probability
#' of a bus being late based on weather and hour. 
#'
#' @param df with at least the columns Delay.Sec, weather_desc, Rounded.Time, 
#' Driver.ID (for mixed effects)   
#' @param sample_size integer # rows to sample (2k, 5k are effective)  
#' @param seed integer random seed used for sampling (default 1560)
#'
#' @return A list containing:
#'   - `sample_df`: sampled and processed data frame used for modeling  
#'   - `model_glm`: fitted weighted GLM (`glm`) object  
#'   - `model_glmer`: fitted weighted GLMM (`glmer`) object  
#'   - `pred_df`: data frame of predicted probabilities for each weather category  
#'   - `plot_glm`: ggplot object of predicted probabilities from GLM  
#'   - `plot_glm_vs_glmer`: ggplot object comparing GLM vs GLMM predicted probabilities  
#'
#' @examples
#' results <- analyze_lateness(df_weather, sample_size = 5000, seed = 1560)
#' summary(results$model_glm)
#' summary(results$model_glmer)
#' results$plot_glm
#' results$plot_glm_vs_glmer

analyze_lateness <- function(df, sample_size = 5000, seed = 1560) {
  stopifnot(all(c("Delay.Sec","weather_desc","Rounded.Time","Driver.ID") %in% names(df)))
  
  # define late & weather_cat again
  df <- df %>%
    mutate(
      late5 = ifelse(Delay.Sec > 5*60, 1, 0),
      weather_cat = case_when(
        grepl("Dense|Heavy|Moderate|Light|Slight|Drizzle|Snow", weather_desc) ~ "precip",
        grepl("Clear|Mainly clear|Partly cloudy|Overcast", weather_desc) ~ "clear",
        TRUE ~ "other"
      )
    ) %>%
    filter(weather_cat != "other") %>%
    mutate(
      weather_cat = factor(weather_cat),
      Driver.ID = factor(Driver.ID)
    )
  
  # sample rows (5k is good)
  if (sample_size > nrow(df)) sample_size <- nrow(df)
  set.seed(seed)
  sample_df <- df %>% sample_n(sample_size)
  
  # IPWs to improve glm significantly
  weather_counts <- sample_df %>%
    count(weather_cat) %>%
    mutate(
      freq = n / sum(n),
      weight = 1 / freq
    )
  
  sample_df <- sample_df %>%
    left_join(weather_counts %>% select(weather_cat, weight), by = "weather_cat")
  
  # making sure we have hour
  sample_df <- sample_df %>% mutate(hour_num = hour(Rounded.Time))
  
  # Weighted GLM
  model_weighted_glm <- glm(late5 ~ weather_cat + hour_num,
                            data = sample_df,
                            family = binomial(),
                            weights = weight)
  
  # Weighted GLMM
  model_weighted_glmer <- glmer(late5 ~ weather_cat + hour_num + (1 | Driver.ID),
                                data = sample_df,
                                family = binomial(),
                                weights = weight,
                                control = glmerControl(optimizer = "bobyqa"))
  
  pred_df <- expand.grid(
    weather_cat = levels(sample_df$weather_cat),
    hour_num = median(sample_df$hour_num)
  )
  # predict probs
  pred_df$prob_glm <- predict(model_weighted_glm, newdata = pred_df, type = "response")
  pred_df$prob_glmer <- predict(model_weighted_glmer, newdata = pred_df, type = "response", re.form = NA)
  
  # data for plotting
  pred_df_long <- pred_df %>%
    pivot_longer(cols = c(prob_glm, prob_glmer), names_to = "model", values_to = "prob")
  
  # plots
  p1 <- ggplot(pred_df, aes(x = weather_cat, y = prob_glm, fill = weather_cat)) +
    geom_col(fill = "grey") +
    labs(
      title = "Predicted probability of being late (>5 min) by weather (GLM)",
      x = "Weather Category",
      y = "Predicted Probability"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  p2 <- ggplot(pred_df_long, aes(x = weather_cat, y = prob, fill = model)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("prob_glm" = "grey", "prob_glmer" = "black")) +
    labs(
      title = "Predicted probability of being late (>5 min) by weather (GLM vs GLMM)",
      x = "Weather Category",
      y = "Predicted Probability",
      fill = "Model"
    ) +
    theme_minimal()
  
  list(
    sample_df = sample_df,
    model_glm = model_weighted_glm,
    model_glmer = model_weighted_glmer,
    pred_df = pred_df,
    plot_glm = p1,
    plot_glm_vs_glmer = p2
  )
}
