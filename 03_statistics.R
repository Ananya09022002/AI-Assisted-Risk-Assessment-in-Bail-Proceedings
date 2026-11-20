analysis_data <- bail |>
  filter(!is.na(post_bail_misconduct))

nrow(analysis_data)

analysis_data |>
  count(post_bail_misconduct)

analysis_data |>
  count(prior_cases, sort = TRUE)

analysis_data |>
  count(charge_sheet_status, sort = TRUE)

analysis_data |>
  count(witness_risk_raised, sort = TRUE)

analysis_data |>
  count(absconding_risk_raised, sort = TRUE)

analysis_data |>
  count(prosecution_opposed_bail, sort = TRUE)

analysis_data |>
  count(bail_type, sort = TRUE)

analysis_data |>
  count(crime_group, sort = TRUE)

analysis_data |>
  count(
    prior_cases,
    post_bail_misconduct
  )

analysis_data |>
  count(
    witness_risk_raised,
    post_bail_misconduct
  )

analysis_data |>
  count(
    absconding_risk_raised,
    post_bail_misconduct
  )

table(
  analysis_data$prior_cases,
  analysis_data$post_bail_misconduct,
  useNA = "ifany"
)

table(
  analysis_data$witness_risk_raised,
  analysis_data$post_bail_misconduct,
  useNA = "ifany"
)

table(
  analysis_data$absconding_risk_raised,
  analysis_data$post_bail_misconduct,
  useNA = "ifany"
)

table(
  analysis_data$prosecution_opposed_bail,
  analysis_data$post_bail_misconduct,
  useNA = "ifany"
)

table(
  analysis_data$charge_sheet_status,
  analysis_data$post_bail_misconduct,
  useNA = "ifany"
)

table(
  analysis_data$bail_type,
  analysis_data$post_bail_misconduct,
  useNA = "ifany"
)

table(
  analysis_data$crime_group,
  analysis_data$post_bail_misconduct,
  useNA = "ifany"
)

analysis_data |>
  group_by(witness_risk_raised) |>
  summarise(
    total = n(),
    misconduct_cases = sum(post_bail_misconduct == 1, na.rm = TRUE),
    misconduct_percent = round(
      mean(post_bail_misconduct == 1, na.rm = TRUE) * 100,
      1
    ),
    .groups = "drop"
  )

analysis_data |>
  group_by(absconding_risk_raised) |>
  summarise(
    total = n(),
    misconduct_cases = sum(post_bail_misconduct == 1, na.rm = TRUE),
    misconduct_percent = round(
      mean(post_bail_misconduct == 1, na.rm = TRUE) * 100,
      1
    ),
    .groups = "drop"
  )

analysis_data |>
  group_by(prior_cases) |>
  summarise(
    total = n(),
    misconduct_cases = sum(post_bail_misconduct == 1, na.rm = TRUE),
    misconduct_percent = round(
      mean(post_bail_misconduct == 1, na.rm = TRUE) * 100,
      1
    ),
    .groups = "drop"
  )


