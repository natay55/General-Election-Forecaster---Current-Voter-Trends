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
  "Green Party"            = "spatial_lag_green",
  "Brexit Party/Reform UK" = "spatial_lag_reform",
  "Liberal Democrat"       = "spatial_lag_ld"
)

# FIXED INDIVIDUAL BASELINES: Dominant, evenly-distributed demographic baselines.
# Large enough across the BES sample to stay safely fixed without causing unobserved cells.
FIXED_DEMO_VARS <- c(
  "gender",              # sex
  "ageGroup",            # Age group of individual
  "p_education_level",   # qualifications — graduate/non-graduate divide
  "housing_tenure_",     # Type of hosuing tenure of an individual
  "ethnicity_harmonised" #Ethnicity of individual
)

# FIXED CONSTITUENCY CONTEXT: Continuous macro-level census variables 
FIXED_CONTEXT_VARS <- c(
  "density",                  # population density — urban/rural divide
  "mortgage_owner_loan_pct",  # Proportion of those home owners with a mortgage or a loan
  "private_rented_pct",       # Proportion of those who are privately renting
  "con_pct",                  # constituency degree holders percentage
  "muslim_pct",               # constituency Muslim population — community political effects
  "is_incumbent",             # Binary indicator for incumbency
  "index"                     # Index of Multiple Deprivation
)

# RANDOM DEMOGRAPHIC INTERCEPTS: Individual demographics and political backgrounds 
# prone to geographic clustering or small/empty cell counts inside individual constituencies.
# Converting these to random effects invokes shrinkage to protect sparse cells from overfitting.
RANDOM_DEMO_VARS <- c(
  "(1 | past_vote_2024)",     #Random effect of past vote
  "(1 | p_eurefvote)"         #Random effect of Brexit vote
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
        vote           = if_else(vote_label == party, 1L, 0L),
        is_incumbent   = if_else(current_winner == party, 1L, 0L)
      )
    
    # Add spatial lag for geographically driven parties
    spatial_var <- spatial_lag_map[[party]]
    fixed_effects <- if (!is.null(spatial_var)) c(FIXED_DEMO_VARS, FIXED_CONTEXT_VARS, spatial_var) else c(FIXED_DEMO_VARS, FIXED_CONTEXT_VARS)
    
    # Construct formula separating true fixed coefficients from partial-pooling random blocks
    formula_str <- paste(
      "vote ~",
      paste(fixed_effects, collapse = " + "), "+",
      paste(RANDOM_DEMO_VARS, collapse = " + "),
      "+ (1 | new_pcon)" # Constituency random intercept baseline
    )
    
    party_models[[party]] <- glmer(
      as.formula(formula_str),
      data    = party_data,
      control = glmerControl(autoscale=TRUE),
      family  = binomial(link = "logit")
    )
    
    cat("Fitted model for:", party, "\n")
  }
  
  saveRDS(party_models, here("data", "Models","England","party_models.rds"))
}