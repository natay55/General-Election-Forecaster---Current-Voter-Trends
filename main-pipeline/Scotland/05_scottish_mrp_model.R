#-------------------------------------------------------------------------------------------
# GLM MRP model specification for Scotland
# Following Lauderdale (2018) and Park, Gelman and Bafumi (2004)
# Separate binary logistic model per party with constituency random effects

party_share_map_scottish <- list(
  "Labour"                        = "Lab24",
  "Conservative"                  = "Con24",
  "Liberal Democrat"              = "LD24",
  "Brexit Party/Reform UK"        = "RUK24",
  "Green Party"                   = "Green24",
  "Scottish National Party (SNP)" = "SNP24",
  "Other"                         = "Other24"
)

FIXED_DEMO_VARS_SCOTLAND <- c(
  "gender",                       # sex
  "ageGroup_scot",                # age group
  "p_education_level",            # qualifications — graduate/non-graduate divide
  "past_vote_2024",               # 2024 general election baseline
  "housing_tenure_"               # individual tenure type (e.g. rent, own home)
)

FIXED_CONTEXT_VARS_SCOTLAND <- c(
  "mortgage_owner_loan_pct",      # Proportion of those home owners with a mortgage or a loan
  "private_rented_pct",           # Proportion of those who are privately renting
  "Con_pc",                       # constituency degree holders percentage
  "scot_rem",                     # voted to remain in Scottish independence referendum
  "is_incumbent",                 # Incumbent indicator for current parties
  "claimant_pct",                 # Percentage of claimants in each constituency
  "pct_disabled",                 # Percentage of disabled in each constituency
  "dep_index"                     # Index of Multiple Deprivation
)

parties_of_interest_scotland <- c(
  "Labour",
  "Conservative",
  "Liberal Democrat",
  "Scottish National Party (SNP)",
  "Green Party",
  "Brexit Party/Reform UK",
  "Other"
)

#-------------------------------------------------------------------------------------------
# Fit or load models

PARTY_MODELS_SCOTLAND_PATH <- here("data", "Models", "Scotland", "party_models_scotland.rds")

if (file.exists(PARTY_MODELS_SCOTLAND_PATH)) {
  party_models_scotland <- readRDS(PARTY_MODELS_SCOTLAND_PATH)
} else {
  party_models_scotland <- list()
  
  for (party in parties_of_interest_scotland) {
    party_data <- voting_likely_scotland |>
      mutate(
        vote         = if_else(vote_label == party, 1L, 0L),
        is_incumbent = if_else(!is.na(current_winner) & current_winner == party, 1L, 0L)
      )
    
    active_context_vars <- FIXED_CONTEXT_VARS_SCOTLAND
    if (sum(party_data$is_incumbent, na.rm = TRUE) == 0) {
      active_context_vars <- setdiff(active_context_vars, "is_incumbent")
    }
    
    fixed_effects_scotland <- c(FIXED_DEMO_VARS_SCOTLAND, active_context_vars)
    
    party_data <- party_data |> 
      drop_na(all_of(c(fixed_effects_scotland, "vote", "new_pcon")))
    
    # 3. Model specification & offset logic
    if (party == "Brexit Party/Reform UK") {
      # Reform uses glmer with 2024 vote share as offset
      # Safe handling of missing values or zeroes in RUK24
      party_data <- party_data |>
        mutate(
          RUK24_clean   = dplyr::coalesce(RUK24, 0),
          RUK24_clamped = pmax(pmin(RUK24_clean, 0.999), 0.001),
          ruk24_offset  = log(RUK24_clamped / (1 - RUK24_clamped))
        ) |>
        drop_na(ruk24_offset)
      
      formula_str_scotland <- paste(
        "vote ~",
        paste(fixed_effects_scotland, collapse = " + "),
        "+ (1 | new_pcon) + offset(ruk24_offset)"
      )
    } else {
      formula_str_scotland <- paste(
        "vote ~",
        paste(fixed_effects_scotland, collapse = " + "),
        "+ (1 | new_pcon)"
      )
    }
    
    party_models_scotland[[party]] <- glmer(
      as.formula(formula_str_scotland),
      data    = party_data,
      control = glmerControl(autoscale = TRUE),
      family  = binomial(link = "logit")
    )
    
    cat("Fitted model for:", party, "\n")
  }
  
  saveRDS(party_models_scotland, PARTY_MODELS_SCOTLAND_PATH)
}