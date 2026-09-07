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
  "gender",             # sex
  "p_education_level",   # qualifications — graduate/non-graduate divide
  "housing_tenure_"    # individual tenure type (e.g. rent, own home)
)

FIXED_CONTEXT_VARS_SCOTLAND <- c(
  "mortgage_owner_loan_pct",      # Proportion of those home owners with a mortgage or a loan
  "private_rented_pct",           # Proportion of those who are privately renting
  "Con_pc",                       # constituency degree holders percentage
  "scot_rem",                     # voted to remain in Scottish independence referendum
  "party_share_24",               # party specific 2024 constituency vote share
  "dep_index"                     # Index of Multiple Deprivation
)

RANDOM_DEMO_VARS_SCOTLAND <- c(
  "(1 | ageGroup_scot)",             
  "(1 | past_vote_2024)"    
)

parties_of_interest_scotland <- c(
  "Labour",
  "Conservative",
  "Liberal Democrat",
  "Green Party",
  "Brexit Party/Reform UK",
  "Scottish National Party (SNP)",
  "Other"
)

#-------------------------------------------------------------------------------------------
# Fit or load models

PARTY_MODELS_SCOTLAND_PATH <- here("data","Models","Scotland","party_models_scotland.rds")

if (file.exists(PARTY_MODELS_SCOTLAND_PATH)) {
  party_models_scotland <- readRDS(PARTY_MODELS_SCOTLAND_PATH)
} else {
  party_models_scotland <- list()
  
  for (party in parties_of_interest_scotland) {
    party_data <- voting_likely_scotland |>
      mutate(
        vote           = if_else(vote_label == party, 1L, 0L),
        raw_share      = if_else(
          !is.na(by_election_share) & current_winner == party,
          by_election_share,
          .data[[party_share_map_scottish[[party]]]]
        ),
        party_share_24 = if_else(
          current_winner == party,
          1 + raw_share,
          raw_share
        )
      )
    
    fixed_effects_scotland <- if (!is.null(spatial_var)) c(FIXED_DEMO_VARS_SCOTLAND, FIXED_CONTEXT_VARS_SCOTLAND) else c(FIXED_DEMO_VARS_SCOTLAND, FIXED_CONTEXT_VARS_SCOTLAND)
    
    if (party == "Brexit Party/Reform UK") {
      # Reform uses glmer with 2024 vote share as offset
      # Offset acts as informative prior on constituency random effect
      # Prevents demographic extrapolation from English patterns
      # with sparse data (mean 7 Reform voters per constituency)
      party_data <- party_data |>
        mutate(
          ruk24_offset = log(
            pmax(RUK24, 0.001) / (1 - pmax(pmin(RUK24, 0.999), 0.001))
          )
        )
      
      formula_str_scotland <- paste(
        "vote ~",
        paste(fixed_effects_scotland, collapse = " + "), "+",
        paste(RANDOM_DEMO_VARS_SCOTLAND, collapse = " + "),
        "+ (1 | new_pcon) + offset(ruk24_offset)"
      )
    } else {
      formula_str_scotland <- paste(
        "vote ~",
        paste(fixed_effects_scotland, collapse = " + "), "+",
        paste(RANDOM_DEMO_VARS_SCOTLAND, collapse = " + "),
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