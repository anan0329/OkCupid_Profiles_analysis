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

ethnicities <- c("asian", "middle eastern", "black", "native american", "indian", 
                 "pacific islander", "hispanic / latin", "white", "other")
ethnicity_vars <- ethnicities %>% 
  purrr::map(~ expr(ifelse(like(ethnicity, !!.x), 1, 0))) %>%
  purrr::set_names(paste0("ethnicity_", gsub("\\s|/", "_", ethnicities)))

okc_train <- okc_train %>%
  mutate(scaled_age = (age - !!scale_values$mean_age) / !!scale_values$sd_age) %>% 
  mutate(!!!ethnicity_vars)
