common_data <- model.frame(model_with_prior)

nrow(common_data)

model_base_same <- glm(
  post_bail_misconduct ~
    witness_risk_raised +
    absconding_risk_raised,
  data = common_data,
  family = binomial
)

model_prior_same <- glm(
  post_bail_misconduct ~
    prior_cases +
    witness_risk_raised +
    absconding_risk_raised,
  data = common_data,
  family = binomial
)

common_data$pred_base <- predict(
  model_base_same,
  type = "response"
)

common_data$pred_prior <- predict(
  model_prior_same,
  type = "response"
)

library(pROC)

roc_base_same <- roc(
  common_data$post_bail_misconduct,
  common_data$pred_base
)

roc_prior_same <- roc(
  common_data$post_bail_misconduct,
  common_data$pred_prior
)

auc(roc_base_same)
auc(roc_prior_same)

roc.test(
  roc_base_same,
  roc_prior_same,
  paired = TRUE,
  method = "delong"
)


