model_prior <- glm(
  post_bail_misconduct ~ prior_cases,
  data = analysis_data,
  family = binomial
)

summary(model_prior)

library(broom)

tidy(
  model_prior,
  exponentiate = TRUE,
  conf.int = TRUE
)


model_witness <- glm(
  post_bail_misconduct ~ witness_risk_raised,
  data = analysis_data,
  family = binomial
)

tidy(
  model_witness,
  exponentiate = TRUE,
  conf.int = TRUE
)


model_absconding <- glm(
  post_bail_misconduct ~ absconding_risk_raised,
  data = analysis_data,
  family = binomial
)

tidy(
  model_absconding,
  exponentiate = TRUE,
  conf.int = TRUE
)

model_prosecution <- glm(
  post_bail_misconduct ~ prosecution_opposed_bail,
  data = analysis_data,
  family = binomial
)

tidy(
  model_prosecution,
  exponentiate = TRUE,
  conf.int = TRUE
)

results_prior <- tidy(
  model_prior,
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  mutate(model = "Prior cases")

results_witness <- tidy(
  model_witness,
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  mutate(model = "Witness risk")

results_absconding <- tidy(
  model_absconding,
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  mutate(model = "Absconding risk")

results_prosecution <- tidy(
  model_prosecution,
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  mutate(model = "Prosecution opposed")


univariable_results <- bind_rows(
  results_prior,
  results_witness,
  results_absconding,
  results_prosecution
)

univariable_results

model_main <- glm(
  post_bail_misconduct ~
    prior_cases +
    witness_risk_raised +
    absconding_risk_raised,
  data = analysis_data,
  family = binomial
)

main_results <- tidy(
  model_main,
  exponentiate = TRUE,
  conf.int = TRUE
)

main_results