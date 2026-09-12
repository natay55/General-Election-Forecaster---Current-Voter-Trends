#-------------------------------------------------------------------------------------------
# GLM MRP model specification
# Following Lauderdale (2018) and Park, Gelman and Bafumi (2004)
# Separate binary logistic model per party with constituency random effects
# Maps each party to its 2024 constituency vote share column
party_share_map_wales <- list(
  "Labour"                 = "Lab24",
  "Conservative"           = "Con24",
  "Liberal Democrat"       = "LD24",
  "Brexit Party/Reform UK" = "RUK24",
  "Green Party"            = "Green24",
  "Plaid Cymru"            = "PC24",
  "Other"                  = "Other24"
)

FIXED_DEMO_VARS_WALES <- c(
  "gender",                             # sex
  "ageGroup",                           # age group of individual voter
  "p_education_level",                  # qualifications — graduate/non-graduate divide
  "housing_tenure_",                    # housing tenure of individual
  "past_vote_2024"                      # 2024 general election vote baseline
)

FIXED_CONTEXT_VARS_WALES <- c(
  "mortgage_owner_loan_pct",            # Proportion of those home owners with a mortgage or a loan
  "private_rented_pct",                 # Proportion of those who are privately renting
  "con_pct",                            # constituency degree holders percentage
  "welsh_speaking",                     # percentage of Welsh speakers in each constituency
  "pct_disabled",                       # Percentage of those disabled under the Equality Act
  "claimant_pct",                       # Percentage of claimants in a constituency
  "index_dep_wales"                     # Index of Multiple Deprivation
)

parties_of_interest_wales <- c(
  "Labour",
  "Conservative",
  "Plaid Cymru",
  "Brexit Party/Reform UK",
  "Liberal Democrat",
  "Green Party",
  "Other"
)

#-------------------------------------------------------------------------------------------
# Fit or load models

if (file.exists(here("data","Models","Wales","party_models_wales.rds"))) {
  party_models_wales <- readRDS(here("data","Models","Wales","party_models_wales.rds"))
} else {
  party_models_wales <- list()
  
  for (party in parties_of_interest_wales) {
    party_data <- voting_likely_wales |>
      mutate(
        vote           = if_else(vote_label == party, 1L, 0L)
      )
    
    fixed_effects_wales <- c(FIXED_DEMO_VARS_WALES, FIXED_CONTEXT_VARS_WALES)
    
    formula_str_wales <- paste(
      "vote ~",
      paste(fixed_effects_wales, collapse = " + "),
      "+ (1 | new_pcon)" 
    )
    
    party_models_wales[[party]] <- glmer(
      as.formula(formula_str_wales),
      data    = party_data,
      control = glmerControl(autoscale = TRUE),
      family  = binomial(link = "logit")
    )
    
    cat("Fitted model for:", party, "\n")
  }
  
  saveRDS(party_models_wales, here("data","Models","Wales","party_models_wales.rds"))
}