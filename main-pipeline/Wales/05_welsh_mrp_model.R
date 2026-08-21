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

# Individual and constituency level predictors
# Individual: demographics and political identity
# Constituency: contextual effects on vote intention
BASE_VARS_WALES <- c(
  "ageGroup",           # age group
  "gender",             # sex
  "p_education_level",  # qualifications — graduate/non-graduate divide
  "housing_tenure_",    # tenure — renter/owner political divide
  "past_vote_2024",     # 2024 GE vote — strongest individual predictor
  "house_rented",       # constituency rental proportion — housing issue block
  "con_pct",            # constituency degree holders percentage
  "party_share_24",     # party specific 2024 constituency vote share
  "welsh_speaking",        # Percentage of Welsh speakers in each constituency
  "index_dep_wales"     # Welsh Index of Multiple Deprivation (WIMD)
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
        vote           = if_else(vote_label == party, 1L, 0L),
        party_share_24 = .data[[party_share_map_wales[[party]]]]
      )
    
    formula_str_wales <- paste(
      "vote ~",
      paste(BASE_VARS_WALES, collapse = " + "),
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