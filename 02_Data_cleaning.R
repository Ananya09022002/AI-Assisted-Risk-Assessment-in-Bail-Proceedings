binary_vars <- c(
  "prior_cases",
  "bail_cancellation_case",
  "prosecution_opposed_bail",
  "witness_risk_raised",
  "absconding_risk_raised",
  "parity_argument_used",
  "bail_conditions_imposed",
  "bias_flag",
  "bail_order_found_legally_infirm",
  "failure_to_appear",
  "absconding_recorded",
  "witness_interference",
  "bail_cancelled_for_breach",
  "post_bail_misconduct"
)

bail <- bail_raw_cleaned |>
  mutate(
    across(
      all_of(binary_vars),
      ~ case_when(
        as.character(.) == "1" ~ 1L,
        as.character(.) == "0" ~ 0L,
        TRUE ~ NA_integer_
      )
    )
  )

bail |>
  count(post_bail_misconduct)

bail |>
  count(post_bail_misconduct) |>
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  )

bail |>
  count(witness_risk_raised, post_bail_misconduct)

bail |>
  count(absconding_risk_raised, post_bail_misconduct)

bail |>
  count(prosecution_opposed_bail, post_bail_misconduct)


bail <- bail |>
  mutate(
    core_post_bail_misconduct = case_when(
      failure_to_appear == 1 |
        absconding_recorded == 1 |
        witness_interference == 1 |
        bail_cancelled_for_breach == 1 ~ 1,
      
      failure_to_appear == 0 &
        absconding_recorded == 0 &
        witness_interference == 0 &
        bail_cancelled_for_breach == 0 ~ 0,
      
      TRUE ~ NA_real_
    )
  )


bail <- bail |>
  mutate(
    broad_post_bail_misconduct = case_when(
      core_post_bail_misconduct == 1 |
        new_case_registered == 1 ~ 1,
      
      core_post_bail_misconduct == 0 &
        new_case_registered == 0 ~ 0,
      
      TRUE ~ NA_real_
    )
  )

bail |>
  count(bail_outcome)

bail |>
  count(post_bail_misconduct, bail_outcome)

bail |>
  summarise(
    total_cases = n(),
    failure_to_appear_yes =
      sum(failure_to_appear == 1, na.rm = TRUE),
    
    absconding_yes =
      sum(absconding_recorded == 1, na.rm = TRUE),
    
    witness_interference_yes =
      sum(witness_interference == 1, na.rm = TRUE),
    
    bail_breach_yes =
      sum(bail_cancelled_for_breach == 1, na.rm = TRUE),
    
    new_case_yes =
      sum(new_case_registered == 1, na.rm = TRUE),
    
    legally_infirm_yes =
      sum(bail_order_found_legally_infirm == 1, na.rm = TRUE)
  )

