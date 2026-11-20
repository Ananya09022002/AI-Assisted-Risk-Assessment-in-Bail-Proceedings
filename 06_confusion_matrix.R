cv_data <- analysis_data |>
  select(
    post_bail_misconduct,
    prior_cases,
    witness_risk_raised,
    absconding_risk_raised
  ) |>
  drop_na()

nrow(cv_data)

cv_data |>
  count(post_bail_misconduct)


library(tidymodels)

cv_data <- cv_data |>
  mutate(
    post_bail_misconduct = factor(
      post_bail_misconduct,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
  )

set.seed(2026)

folds <- vfold_cv(
  cv_data,
  v = 5,
  repeats = 10,
  strata = post_bail_misconduct
)

count(cv_data)

log_model <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

log_recipe <- recipe(
  post_bail_misconduct ~
    prior_cases +
    witness_risk_raised +
    absconding_risk_raised,
  data = cv_data
)

log_workflow <- workflow() |>
  add_recipe(log_recipe) |>
  add_model(log_model)

cv_results <- fit_resamples(
  log_workflow,
  resamples = folds,
  metrics = metric_set(
    roc_auc,
    accuracy,
    sensitivity,
    specificity
  ),
  control = control_resamples(
    save_pred = TRUE
  )
)

collect_metrics(cv_results)

cv_data |>
  count(post_bail_misconduct) |>
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  )

base_recipe <- recipe(
  post_bail_misconduct ~
    witness_risk_raised +
    absconding_risk_raised,
  data = cv_data
)

base_workflow <- workflow() |>
  add_recipe(base_recipe) |>
  add_model(log_model)


base_cv_results <- fit_resamples(
  base_workflow,
  resamples = folds,
  metrics = metric_set(
    roc_auc,
    accuracy,
    sensitivity,
    specificity
  ),
  control = control_resamples(
    save_pred = TRUE
  )
)

collect_metrics(base_cv_results)

cv_predictions <- collect_predictions(cv_results)

names(cv_predictions)

cv_predictions |>
  conf_mat(
    truth = post_bail_misconduct,
    estimate = .pred_class
  )

cv_predictions |>
  metric_set(
    accuracy,
    sensitivity,
    specificity,
    precision,
    recall,
    f_meas
  )(
    truth = post_bail_misconduct,
    estimate = .pred_class
  )
