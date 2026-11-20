model_primary <- glm(
  post_bail_misconduct ~
    witness_risk_raised +
    absconding_risk_raised,
  data = analysis_data,
  family = binomial
)

nobs(model_primary)

library(broom)

tidy(
  model_primary,
  exponentiate = TRUE,
  conf.int = TRUE
)

model_prior_data <- analysis_data |>
  select(
    post_bail_misconduct,
    prior_cases,
    witness_risk_raised,
    absconding_risk_raised
  ) |>
  drop_na()

nrow(model_prior_data)


model_with_prior <- glm(
  post_bail_misconduct ~
    prior_cases +
    witness_risk_raised +
    absconding_risk_raised,
  data = model_prior_data,
  family = binomial
)

tidy(
  model_with_prior,
  exponentiate = TRUE,
  conf.int = TRUE
)

analysis_data <- analysis_data |>
  mutate(
    prior_history_available = if_else(
      is.na(prior_cases),
      0,
      1
    )
  )

analysis_data |>
  count(
    prior_history_available,
    post_bail_misconduct
  )

analysis_data |>
  group_by(prior_history_available) |>
  summarise(
    cases = n(),
    misconduct = sum(post_bail_misconduct == 1),
    misconduct_rate = round(
      mean(post_bail_misconduct == 1) * 100,
      1
    ),
    .groups = "drop"
  )

analysis_data |>
  count(accused_gender, prior_history_available)

analysis_data |>
  count(region, prior_history_available)

analysis_data |>
  count(crime_group, prior_history_available)


primary_eval <- model.frame(model_primary)

primary_eval$predicted_probability <- predict(
  model_primary,
  type = "response"
)

nrow(primary_eval)
length(primary_eval$predicted_probability)


library(pROC)

roc_primary <- roc(
  primary_eval$post_bail_misconduct,
  primary_eval$predicted_probability
)

auc(roc_primary)

ci.auc(roc_primary)

plot(
  roc_primary,
  main = "ROC Curve: Primary Post-Bail Misconduct Model"
)

secondary_eval <- model.frame(model_with_prior)

secondary_eval$predicted_probability <- predict(
  model_with_prior,
  type = "response"
)

roc_secondary <- roc(
  secondary_eval$post_bail_misconduct,
  secondary_eval$predicted_probability
)

auc(roc_secondary)
ci.auc(roc_secondary)

plot(
  roc_secondary,
  main = "ROC Curve: Secondary Post-Bail Misconduct Model"
)

nrow(secondary_eval)
