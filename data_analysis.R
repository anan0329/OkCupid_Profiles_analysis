library(sparklyr)
library(ggplot2)
library(dbplot)
library(dplyr)

sc <- spark_connect(master = "local")

okc <- spark_read_csv(
  sc, 
  "data/profiles.csv", 
  escape = "\"", 
  memory = FALSE,
  options = list(multiline = TRUE)
) %>%
  mutate(
    height = as.numeric(height),
    income = ifelse(income == "-1", NA, as.numeric(income))
  ) %>% 
  mutate(body_type = ifelse(is.na(body_type), "missing", sex)) %>%
  mutate(sex = ifelse(is.na(sex), "missing", sex)) %>%
  mutate(drinks = ifelse(is.na(drinks), "missing", drinks)) %>%
  mutate(drugs = ifelse(is.na(drugs), "missing", drugs)) %>%
  mutate(job = ifelse(is.na(job), "missing", job)) %>% 
  mutate(not_working = ifelse(job %in% c("student", "unemployed", "retired"), 1 , 0)
  
data_splits <- sdf_random_split(okc, training = 0.8, testing = 0.2, seed = 420)
okc_train <- data_splits$training
okc_test <- data_splits$testing

scale_values <- okc_train %>%
  summarize(mean_age = mean(age), sd_age = sd(age)) %>%
  collect()

ethnic <- c("asian", "middle eastern", "black", "native american", "indian", 
                 "pacific islander", "hispanic / latin", "white", "other")
ethnic_vars <- ethnic %>% 
  purrr::map(~ expr(ifelse(like(ethnic, !!.x), 1, 0))) %>%
  purrr::set_names(paste0("ethnic_", gsub("\\s|/", "_", ethnic)))

okc_train <- okc_train %>%
  mutate(scaled_age = (age - !!scale_values$mean_age) / !!scale_values$sd_age) %>% 
  mutate(!!!ethnicity_vars)

vfolds <- sdf_random_split(okc_train, weights = purrr::set_names(rep(0.1, 10), paste0("fold", 1:10)), seed = 420)

make_scale_age <- function(tr_ana) {
  scale_values <- tr_ana %>%
    summarize(
      mean_age = mean(age),
      sd_age = sd(age)
    ) %>%
    collect()
  
  function(data) {
    mutate(data, scaled_age = (age - !!scale_values$mean_age) / !!scale_values$sd_age)
  }
}
         
cv_results <- purrr::map_df(1:10, function(v) {
  tr_ana <- do.call(rbind, vfolds[setdiff(1:10, v)]) %>% compute()
  val_ana <- vfolds[[v]]
  
  scale_age <- make_scale_age(tr_ana)
  re_set <- scale_age(tr_ana)
  val_set <- scale_age(val_ana)
  
  model <- ml_logistic_regression(
    analysis_set, not_working ~ scaled_age + sex + drinks + drugs + essay_length
  )
  s <- ml_evaluate(model, assessment_set)
  roc_df <- s$roc() %>% 
    collect()
  auc <- s$area_under_roc()
  
  tibble(
    Resample = paste0("Fold", stringr::str_pad(v, width = 2, pad = "0")),
    roc_df = list(roc_df),
    auc = auc
  )
})
