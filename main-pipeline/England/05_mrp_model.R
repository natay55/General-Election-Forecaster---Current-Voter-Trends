#-------------------------------------------------------------------------------------------
# GLM MRP model specification
# Following Lauderdale (2018) and Park, Gelman and Bafumi (2004)
# Separate binary logistic model per party with constituency random effects
# Maps each party to its 2024 constituency vote share column
party_share_map <- list(
  "Labour"                 = "Lab24",
  "Conservative"           = "Con24",
  "Liberal Democrat"       = "LD24",
  "Brexit Party/Reform UK" = "RUK24",
  "Green Party"            = "Green24",
  "Other"                  = "Other24"
)

# Spatial lag predictors applied only to geographically driven parties
# LD and Green support reflects place effects beyond demographics
# applying spatial lags to all parties but only including in formula
# for parties where geographic clustering drives support beyond demographics
spatial_lag_map <- list(
  "Labour"                 = "spatial_lag_lab",
  "Conservative"           = "spatial_lag_con",
  "Brexit Party/Reform UK" = "spatial_lag_reform"
)

# FIXED INDIVIDUAL BASELINES: Dominant, evenly-distributed demographic baselines.
# Large enough across the BES sample to stay safely fixed without causing unobserved cells.
FIXED_DEMO_VARS <- c(
  "gender",                   # sex
  "ageGroup",                 # Age group of individual
  "p_education_level",        # qualifications — graduate/non-graduate divide
  "housing_tenure_",          # Type of hosuing tenure of an individual
  "ethnicity_harmonised",     # Ethnicity of individual
  "past_vote_2024"            # Past vote in 2024 GE
)

# FIXED CONSTITUENCY CONTEXT: Continuous macro-level census variables 
FIXED_CONTEXT_VARS <- c(
  "density",                  # population density — urban/rural divide
  "mortgage_owner_loan_pct",  # Proportion of those home owners with a mortgage or a loan
  "private_rented_pct",       # Proportion of those who are privately renting
  "con_pct",                  # constituency degree holders percentage
  "is_incumbent",             # Binary indicator for incumbency
  "vote_share",               # Vote share in 2024 general election
  "remain",                   # Hanretty estimates of remain voters for Brexit, capturing immigration attitudes
  "muslim_pct",               # Percentage of Muslims in a constituency
  "claimant_pct",             # Percentage of disabled under the Equality Act by constituency
  "pct_disabled",             # Percentage of claimants in each constituency
  "is_high_profile",
  "index"                     # Index of Multiple Deprivation
)

INTERACTION_MAP <- list(
  "Labour" = c(
    "ageGroup:density",
    "p_education_level:density",
    "housing_tenure_:private_rented_pct",
    "past_vote_2024:remain",
    "p_education_level:index",
    "ageGroup:claimant_pct"
  ),
  
  "Brexit Party/Reform UK" = c(
    "p_education_level:remain",
    "past_vote_2024:remain",
    "ageGroup:claimant_pct",
    "housing_tenure_:claimant_pct",
    "p_education_level:index",
    "ageGroup:index"
  ),
  
  "Conservative" = c(
    "past_vote_2024:is_incumbent",
    "ageGroup:con_pct",
    "housing_tenure_:mortgage_owner_loan_pct"
  ),
  
  "Liberal Democrat" = c(
    "p_education_level:remain",
    "p_education_level:density",
    "ageGroup:con_pct"
  ),
  
  "Green Party" = c(
    "p_education_level:density",
    "ageGroup:density"
  ),
  
  "Other" = c(
    "past_vote_2024:muslim_pct",
    "past_vote_2024:claimant_pct"
  )
)

high_profile <- list(
  "chorley"           = "Other",
  "makerfield"        = "Labour",
  "gorton and denton" = "Green Party",
  "islington north"   = "Other",
  "great yarmouth"    = "Other"
)

parties_of_interest <- c(
  "Labour",
  "Conservative",
  "Liberal Democrat",
  "Green Party",
  "Brexit Party/Reform UK",
  "Other"
)

#-------------------------------------------------------------------------------------------
# Fit or load models

if (file.exists(here("data", "Models","England","party_models.rds"))) {
  party_models <- readRDS(here("data", "Models","England","party_models.rds"))
} else {
  party_models <- list()
  
  for (party in parties_of_interest) {
    
    party_data <- voting_likely_england |>
      mutate(
        vote            = if_else(vote_label == party, 1L, 0L),
        is_incumbent    = if_else(current_winner == party, 1L, 0L),
        vote_share      = if_else(
          !is.na(by_election_share) & current_winner == party,
          by_election_share,
          .data[[party_share_map[[party]]]]
        ),
        is_high_profile = if_else(
          new_pcon %in% names(high_profile) & unname(high_profile[new_pcon]) == party,
          1L,
          0L,
          missing = 0L
        )
      )
    
    # Check if this specific party actually has any high-profile seats (sum > 0)
    has_hp_seats <- sum(party_data$is_high_profile, na.rm = TRUE) > 0
    
    # Remove is_high_profile from the context vars if the party has none
    party_context_vars <- FIXED_CONTEXT_VARS
    if (!has_hp_seats) {
      party_context_vars <- setdiff(party_context_vars, "is_high_profile")
    }
    
    spatial_var      <- spatial_lag_map[[party]]
    interaction_vars <- INTERACTION_MAP[[party]]
    
    fixed_effects <- c(
      FIXED_DEMO_VARS,
      party_context_vars,
      interaction_vars,
      spatial_var
    )
    fixed_effects <- fixed_effects[!is.na(fixed_effects)]
    
    formula_str <- paste(
      "vote ~",
      paste(fixed_effects, collapse = " + "),
      "+ (1 | new_pcon)"
    )
    
    party_models[[party]] <- glmer(
      as.formula(formula_str),
      data = party_data,
      control = glmerControl(autoscale = TRUE),
      family = binomial(link = "logit")
    )
    
    cat("Fitted model for:", party, "\n")
  }
  
  saveRDS(party_models, here("data", "Models","England","party_models.rds"))
}