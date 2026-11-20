packages <- c(
  "tidyverse",
  "tidymodels",
  "janitor",
  "skimr",
  "naniar",
  "broom",
  "textrecipes",
  "tidytext",
  "stopwords",
  "glmnet",
  "ranger",
  "xgboost",
  "vip",
  "pROC",
  "probably",
  "yardstick",
  "fairness",
  "themis",
  "readxl"
)

new_packages <- packages[!packages %in% installed.packages()[, "Package"]]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

library(tidyverse)
library(tidymodels)
library(janitor)
library(skimr)
library(tidytext)
library(readxl)

tidymodels_prefer()
set.seed(2026)

bail_raw_cleaned <- read_excel("data/bail_data_cleaned.xlsx") |>
  clean_names()

dim(bail_raw_cleaned)
glimpse(bail_raw_cleaned)

