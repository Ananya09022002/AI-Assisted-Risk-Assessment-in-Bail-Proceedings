analysis_data |>
  count(crime_group, sort = TRUE)

analysis_data |>
  count(bail_order_found_legally_infirm, sort = TRUE)

infirmity_table <- table(
  analysis_data$crime_group,
  analysis_data$bail_order_found_legally_infirm
)

infirmity_table

analysis_data |>
  filter(!is.na(bail_order_found_legally_infirm)) |>
  group_by(crime_group) |>
  summarise(
    total = n(),
    legally_infirm = sum(
      bail_order_found_legally_infirm == 1
    ),
    infirm_percent = round(
      mean(bail_order_found_legally_infirm == 1) * 100,
      1
    ),
    .groups = "drop"
  ) |>
  arrange(desc(infirm_percent))

chi_result <- janitor::chisq.test(infirmity_table)

chi_result

chi_result$expected

janitor::fisher.test(infirmity_table)


install.packages("effectsize")


library(effectsize)

cramers_v(infirmity_table)

infirm_model <- glm(
  bail_order_found_legally_infirm ~ crime_group,
  data = analysis_data,
  family = binomial
)

library(broom)

tidy(
  infirm_model,
  exponentiate = TRUE,
  conf.int = TRUE
)

levels(factor(analysis_data$crime_group))

analysis_data <- analysis_data |>
  mutate(
    crime_group = relevel(
      factor(crime_group),
      ref = "Other"
    )
  )

analysis_data <- analysis_data |>
  mutate(
    cancellation_basis = case_when(
      bail_order_found_legally_infirm == 1 &
        bail_cancelled_for_breach == 0 ~
        "Legal infirmity",
      
      bail_order_found_legally_infirm == 0 &
        bail_cancelled_for_breach == 1 ~
        "Post-bail breach",
      
      bail_order_found_legally_infirm == 1 &
        bail_cancelled_for_breach == 1 ~
        "Both",
      
      TRUE ~ "Other / None"
    )
  )

analysis_data |>
  count(cancellation_basis)

print(analysis_data |> count(crime_group, cancellation_basis), n = 22)


analysis_data <- analysis_data |>
  mutate(
    legal_infirmity_any = case_when(
      cancellation_basis %in% c(
        "Legal infirmity",
        "Both"
      ) ~ 1,
      
      cancellation_basis %in% c(
        "Post-bail breach",
        "Other / None"
      ) ~ 0,
      
      TRUE ~ NA_real_
    )
  )

analysis_data |>
  count(legal_infirmity_any)

infirmity_data <- analysis_data |>
  filter(
    !is.na(crime_group),
    !is.na(legal_infirmity_any)
  )

crime_infirmity_summary <- infirmity_data |>
  group_by(crime_group) |>
  summarise(
    total_cases = n(),
    legally_infirm = sum(legal_infirmity_any == 1),
    not_legally_infirm = sum(legal_infirmity_any == 0),
    infirmity_percent = round(
      mean(legal_infirmity_any == 1) * 100,
      1
    ),
    .groups = "drop"
  ) |>
  arrange(desc(infirmity_percent))

crime_infirmity_summary

infirmity_table <- table(
  infirmity_data$crime_group,
  infirmity_data$legal_infirmity_any
)

infirmity_table

chi_result <- janitor::chisq.test(infirmity_table)

chi_result

chi_result$expected

set.seed(2026)

fisher_result <- janitor::fisher.test(
  infirmity_table,
  simulate.p.value = TRUE,
  B = 10000
)

fisher_result

install.packages("effectsize")

library(effectsize)
library(rstatix)

cramer_v(infirmity_table)
