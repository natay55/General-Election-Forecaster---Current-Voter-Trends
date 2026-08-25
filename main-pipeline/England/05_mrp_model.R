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
  "Labour"           = "spatial_lag_lab",
  "Conservative"     = "spatial_lag_con",
  "Liberal Democrat" = "spatial_lag_ld",
  "Green Party"      = "spatial_lag_green"
)

# Individual and constituency level predictors
# Individual: demographics and political identity
# Constituency: contextual effects on vote intention
BASE_VARS <- c(
  "ageGroup",           # age group — strong predictor of vote intention
  "ethnicity_harmonised", # ethnicity — individual level,
  "p_eurefvote",         #Brexit vote (leave or remain)
  "gender",             # sex
  "p_education_level",  # qualifications — graduate/non-graduate divide
  "housing_tenure_",    # tenure — renter/owner political divide
  "past_vote_2024",     # 2024 GE vote — strongest individual predictor
  "density",            # population density — urban/rural divide
  "mortgage_owner_loan_pct",     # Proportion of those home owners with a mortgage or a loan
  "private_rented_pct",     # Proportion of those who are privately renting
  "con_pct",            # constituency degree holders percentage
  "muslim_pct",         # constituency Muslim population — community political effects
  "party_share_24",     # party specific 2024 constituency vote share,
  "index"               # Index of Multiple Deprivation
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
        raw_share      = if_else(
          !is.na(by_election_share) & current_winner == party,
          by_election_share,
          .data[[party_share_map[[party]]]]
        ),
        # Boost incumbent party share above 1, non-incumbents stay below 1
        party_share_24 = if_else(
          current_winner == party,
          1 + raw_share,
          raw_share
        )
      )
    
    # Add spatial lag for geographically driven parties
    spatial_var <- spatial_lag_map[[party]]
    all_vars    <- if (!is.null(spatial_var)) c(BASE_VARS, spatial_var) else BASE_VARS
    
    formula_str <- paste(
      "vote ~",
      paste(all_vars, collapse = " + "),
      "+ (1 | new_pcon)"
    )
    
    party_models[[party]] <- glmer(
      as.formula(formula_str),
      data    = party_data,
      control = glmerControl(autoscale = TRUE),
      family  = binomial(link = "logit")
    )
    
    cat("Fitted model for:", party, "\n")
  }
  
  saveRDS(party_models, here("data", "Models","England","party_models.rds"))
}